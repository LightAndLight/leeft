{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Compile where

import Bound.Scope (toScope, fromScope)
import Bound.Var (Var(..), unvar)
import Control.Monad (unless)
import Control.Monad.State (MonadState, runState, get, gets, put, modify)
import Data.Bifunctor (bimap, second)
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Void (absurd)

import Lambda
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Grin.Grin as Grin (packName)
import qualified Grin.Syntax as Grin (Exp(..), Name, Val(..), Lit(..))
import qualified Pipeline.Pipeline as Grin (optimize)
import qualified Pipeline.Definitions as Grin (PipelineStep(..), defaultOpts)
import qualified Transformations.GenerateEval as Grin (generateEval)

-- | In GRIN, unknown application, like `g x` in `\f g x -> f (g x)` need
-- their own node so that we can defer their evaluation.
--
-- This function creates a family of 'ap' functions, for each arity of
-- unknown application, and replaces raw applications to calls to these
-- functions
unknownApps
  :: forall a
   . (Int -> a) -- ^ naming function
  -> [Lifted a]
  -> [(a, Expr a)]
unknownApps name defs =
  let
    (defs', defs'') = runState (go defs) IntMap.empty
  in
    toList defs'' <> defs'
  where
    mkAp :: Int -> (a, Expr a)
    mkAp n =
      ( name n
      , Lam (n+1) $ toScope $
        Call (Var $ B 0) (fmap (Var . B) $ 1 :| [2..n])
      )

    go
      :: MonadState (IntMap (a, Expr a)) m
      => [Lifted a]
      -> m [(a, Expr a)]
    go [] = pure []
    go (Lifted n e:xs) =
      (:) <$> ((,) n <$> goExpr id (absurd <$> e)) <*> go xs

    goExpr
      :: forall b m
       . MonadState (IntMap (a, Expr a)) m
      => (a -> b)
      -> Expr b
      -> m (Expr b)
    goExpr _ (Var n) = pure $ Var n
    goExpr _ (Int n) = pure $ Int n
    goExpr ctx (Call f xs) = do
      let n = length xs
      isMember <- gets $ IntMap.member n
      unless isMember $ modify (IntMap.insert n $ mkAp n)
      fmap (Call $ Var (ctx $ name n)) $
        NonEmpty.cons <$> goExpr ctx f <*> traverse (goExpr ctx) xs
    goExpr ctx (Lam n s) = Lam n . toScope <$> goExpr (F . ctx) (fromScope s)
    goExpr ctx (Add a b) = Add <$> goExpr ctx a <*> goExpr ctx b

defsToGrin
  :: MonadState [a] m
  => (a -> Grin.Name)
  -> [(a, Expr a)]
  -> m [(Grin.Name, [Grin.Name], Grin.Exp)]
defsToGrin name ls = traverse (defToGrin name) ls

toVal
  :: MonadState [a] m
  => (a -> Grin.Name)
  -> (b -> Grin.Name)
  -> Expr b
  -> m (Either (Grin.Exp -> Grin.Exp, Grin.Val) Grin.Val)
toVal _ _ (Int n) = pure . Right $ Grin.Lit (Grin.LInt64 n)
toVal _ nameB (Var a) = pure $ Right (Grin.Var $ nameB a)
toVal nameA nameB e@Add{} = do
  x <- do; xs <- get; nameA (head xs) <$ put (tail xs)
  e' <- exprToGrin nameA nameB e
  pure $ Left (Grin.EBind e' (Grin.Var x), Grin.Var x)
toVal nameA nameB e@Call{} = do
  x <- do; xs <- get; nameA (head xs) <$ put (tail xs)
  e' <- exprToGrin nameA nameB e
  pure $ Left (Grin.EBind e' (Grin.Var x), Grin.Var x)
toVal _ _ Lam{} =
  error "lambdas should not exist in value positions after lifting"

defToGrin
  :: MonadState [a] m
  => (a -> Grin.Name)
  -> (a, Expr a)
  -> m (Grin.Name, [Grin.Name], Grin.Exp)
defToGrin name (n, def) =
  uncurry ((,,) $ name n) <$>
  case def of
    Lam arity s -> do
      names <- do; (xs, xs') <- gets (splitAt arity); fmap name xs <$ put xs'
      s' <- exprToGrin name (unvar (names !!) name) $ fromScope s
      pure (names, s')
    _ -> (,) [] <$> exprToGrin name name def

collapse
  :: [Either (Grin.Exp -> Grin.Exp, Grin.Val) Grin.Val]
  -> (Grin.Exp -> Grin.Exp, [Grin.Val])
collapse [] = (id, [])
collapse (Left (g, x) : rest) = bimap (. g) (x :) (collapse rest)
collapse (Right x : rest) = second (x :) (collapse rest)

exprToGrin
  :: MonadState [a] m
  => (a -> Grin.Name)
  -> (b -> Grin.Name)
  -> Expr b
  -> m Grin.Exp
exprToGrin _ _ (Int n) = pure $ Grin.SReturn (Grin.Lit $ Grin.LInt64 n)
exprToGrin nameA nameB (Add a b) = do
  (g, xs) <- collapse <$> traverse (toVal nameA nameB) [a, b]
  pure . g $ Grin.SApp (Grin.packName "_prim_int_add") xs
exprToGrin _ nameB (Var a) = pure $ Grin.SReturn (Grin.Var $ nameB a)
exprToGrin _ _ Lam{} = error "nested lambdas should have been eliminated"
exprToGrin nameA nameB (Call f xs) = do
  f' <- exprToGrin nameA nameB f
  (g, xs') <- collapse . NonEmpty.toList <$> traverse (toVal nameA nameB) xs
  case f' of
    Grin.SApp name vals -> pure . g . Grin.SApp name $ vals <> xs'
    Grin.SReturn (Grin.Var name) -> pure . g $ Grin.SApp name xs'
    _ -> error "called something weird"

exprToProgram
  :: (IsString a, Semigroup a, Eq a, MonadState [a] m)
  => (a -> Grin.Name)
  -> Expr a
  -> m Grin.Exp
exprToProgram name e = do
  (e', defs) <- liftLambdas e
  defs' <- defsToGrin name $ unknownApps (("ap"<>) . fromString . show) defs
  e'' <- exprToGrin name name e'
  let
    p = Grin.Program $
      fmap (\(a, b, c) -> Grin.Def a b c) defs' <>
      [Grin.Def (Grin.packName "grinMain") [] e'']
  pure $ Grin.generateEval p

optimizeProgram :: Grin.Exp -> IO Grin.Exp
optimizeProgram e =
  Grin.optimize
    Grin.defaultOpts
    e
    []
    [Grin.PrintAST, Grin.SaveLLVM True "out"]
