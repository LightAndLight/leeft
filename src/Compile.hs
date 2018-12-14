{-# language FlexibleContexts #-}
module Compile where

import Bound.Scope (fromScope)
import Bound.Var (unvar)
import Control.Monad.State (MonadState, get, gets, put)
import Control.Monad.Writer (runWriterT)
import Data.Bifunctor (bimap, second)
import Data.Void (absurd)

import Lambda
import qualified Data.List.NonEmpty as NonEmpty
import qualified Grin.Grin as Grin (packName)
import qualified Grin.Syntax as Grin (Exp(..), Name, Val(..), Lit(..))
import qualified Pipeline.Pipeline as Grin (optimize)
import qualified Pipeline.Definitions as Grin (PipelineStep(..), defaultOpts)

liftedsToGrin
  :: MonadState [a] m
  => (a -> Grin.Name)
  -> [Lifted a]
  -> m [(Grin.Name, [Grin.Name], Grin.Exp)]
liftedsToGrin name ls = traverse (liftedToGrin name) ls

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

liftedToGrin
  :: MonadState [a] m
  => (a -> Grin.Name)
  -> Lifted a
  -> m (Grin.Name, [Grin.Name], Grin.Exp)
liftedToGrin name (Lifted n def) =
  uncurry ((,,) $ name n) <$>
  case def of
    Lam arity s -> do
      names <- do; (xs, xs') <- gets (splitAt arity); fmap name xs <$ put xs'
      s' <- exprToGrin name (unvar (names !!) absurd) $ fromScope s
      pure (names, s')
    Var a -> absurd a
    Call f _ ->
      case f of
        Var f' -> absurd f'
        _ -> error "values applied to non-variable"
    _ -> (,) [] <$> exprToGrin name absurd def

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
-- exprToGrin _ nameB (Var a) = pure $ Grin.SReturn (Grin.Var $ nameB a)
exprToGrin _ nameB (Var a) = pure $ Grin.SApp (nameB a) []
exprToGrin _ _ Lam{} = error "nested lambdas should have been eliminated"
exprToGrin nameA nameB (Call f xs) = do
  f' <- exprToGrin nameA nameB f
  (g, xs') <- collapse . NonEmpty.toList <$> traverse (toVal nameA nameB) xs
  case f' of
    Grin.SApp name vals -> pure . g . Grin.SApp name $ vals <> xs'
    -- Grin.SReturn (Grin.Var name) -> pure . g $ Grin.SApp name xs'
    _ -> error "called something weird"

exprToProgram
  :: (Eq a, MonadState [a] m)
  => (a -> Grin.Name)
  -> Expr a
  -> m Grin.Exp
exprToProgram name e = do
  (e', defs) <- runWriterT $ liftLambdas id e
  defs' <- liftedsToGrin name defs
  e'' <- exprToGrin name name e'
  pure . Grin.Program $
    fmap (\(a, b, c) -> Grin.Def a b c) defs' <>
    [Grin.Def (Grin.packName "grinMain") [] e'']

optimizeProgram :: Grin.Exp -> IO Grin.Exp
optimizeProgram e =
  Grin.optimize
    Grin.defaultOpts
    e
    []
    [Grin.PrintAST, Grin.SaveLLVM True "out"]
