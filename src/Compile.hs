{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
module Compile where

import Bound.Scope (fromScope)
import Bound.Var (unvar)
import Control.Monad.Cont (ContT(..))
import Control.Monad.State (MonadState, gets, put)
import Data.Bifunctor (bimap, second)
import Data.Foldable (toList)
import Data.String (IsString(..))
import Data.Void (absurd)

import Lambda
import qualified Grin.Grin as Grin (packName)
import qualified Grin.Syntax as Grin
  (Exp(..), Name, Val(..), Lit(..), pattern SFetch)
import qualified Grin.SyntaxDefs as Grin (Tag(..), TagType(..))
import qualified Pipeline.Pipeline as Grin (optimize)
import qualified Pipeline.Definitions as Grin (PipelineStep(..), defaultOpts)
import qualified Transformations.GenerateEval as Grin (generateEval)

arities :: Eq a => [Lifted a] -> a -> Maybe Int
arities [] _ = Nothing
arities (Lifted a (Lam n _):xs) a' =
  if a == a'
  then Just n
  else arities xs a'
arities (Lifted a _:xs) a' =
  if a == a'
  then Just 0
  else arities xs a'

defsToGrin
  :: (MonadState [a] m, Eq a)
  => (a -> Grin.Name)
  -> [(a, Expr a)]
  -> m [(Grin.Name, [Grin.Name], Grin.Exp)]
defsToGrin name ls = traverse (defToGrin name) ls

liftedToGrin
  :: MonadState [a] m
  => (a -> Grin.Name)
  -> Lifted a
  -> m (Grin.Name, [Grin.Name], Grin.Exp)
liftedToGrin name (Lifted n def) = defToGrin name (n, absurd <$> def)

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
      s' <-
        runContT
          (exprToGrin (const Nothing) name (unvar (names !!) name) $ fromScope s)
          (pure . Grin.SReturn . Grin.Var)
      pure (names, s')
    _ -> do
      s' <-
        runContT
          (exprToGrin (const Nothing) name name def)
          (pure . Grin.SReturn . Grin.Var)
      pure ([], s')

collapse
  :: [Either (Grin.Exp -> Grin.Exp, Grin.Val) Grin.Val]
  -> (Grin.Exp -> Grin.Exp, [Grin.Val])
collapse [] = (id, [])
collapse (Left (g, x) : rest) = bimap (. g) (x :) (collapse rest)
collapse (Right x : rest) = second (x :) (collapse rest)

yieldExp
  :: MonadState [a] m
  => (a -> Grin.Name)
  -> Grin.Exp
  -> ContT Grin.Exp m Grin.Name
yieldExp nameA ea =
  ContT $ \f -> do
    n <- nameA <$> freshName
    Grin.EBind ea (Grin.Var n) <$> f n

cInt :: Grin.Val -> Grin.Val
cInt =
  Grin.ConstTagNode (Grin.Tag Grin.C $ Grin.packName "Int") .
  pure

evalAs
  :: MonadState [a] m
  => (a -> Grin.Name)
  -> (Grin.Name -> Grin.Val)
  -> Grin.Val
  -> ContT Grin.Exp m Grin.Name
evalAs nameA as xs =
  ContT $ \f -> do
    n <- nameA <$> freshName
    Grin.EBind (Grin.SApp (Grin.packName "eval") [xs]) (as n) <$> f n

exprToGrin
  :: MonadState [a] m
  => (b -> Maybe Int)
  -> (a -> Grin.Name)
  -> (b -> Grin.Name)
  -> Expr b
  -> ContT Grin.Exp m Grin.Name
exprToGrin _ nameA _ (Int n) =
  yieldExp nameA $
  Grin.SStore $
  cInt (Grin.Lit $ Grin.LInt64 n)
exprToGrin ars nameA nameB (Add a b) = do
  a' <-
    evalAs nameA (cInt . Grin.Var) .
    Grin.Var =<< exprToGrin ars nameA nameB a
  b' <-
    evalAs nameA (cInt . Grin.Var) .
    Grin.Var =<< exprToGrin ars nameA nameB b
  c <-
    yieldExp
    nameA
    (Grin.SApp (Grin.packName "_prim_int_add") [Grin.Var a', Grin.Var b'])
  yieldExp nameA $ Grin.SStore $ cInt $ Grin.Var c
exprToGrin ars nameA nameB (Var a) =
  case ars a of
    Nothing -> yieldExp nameA $ Grin.SFetch $ nameB a
    Just ar ->
      yieldExp nameA $
      Grin.SStore $
      Grin.ConstTagNode (Grin.Tag (Grin.P ar) (nameB a)) []
exprToGrin _ _ _ Lam{} = error "nested lambdas should have been eliminated"
exprToGrin ars nameA nameB (Call (Var a) xs) = do
  xs' <- traverse (exprToGrin ars nameA nameB) (toList xs)
  case ars a of
    Nothing -> yieldExp nameA $ Grin.SApp (nameB a) (Grin.Var <$> xs')
    Just ar -> do
      let missing = ar - length xs
      if missing == 0
        then
          yieldExp nameA . Grin.SStore $
            Grin.ConstTagNode
              (Grin.Tag Grin.F $ nameB a)
              (Grin.Var <$> xs')
        else
          yieldExp nameA . Grin.SStore $
            Grin.ConstTagNode
              (Grin.Tag (Grin.P missing) $ nameB a)
              (Grin.Var <$> xs')
exprToGrin _ _ _ Call{} =
  error "applications to non-named things should have been eliminated"

exprToProgram
  :: (IsString a, Semigroup a, Eq a, MonadState [a] m)
  => (a -> Grin.Name)
  -> Expr a
  -> m Grin.Exp
exprToProgram name e = do
  (e1, defs1) <- defunctionalize e
  (e2, defs2) <- defunctionalize e1
  defs3 <- traverse (liftedToGrin name) $ defs1 <> defs2
  e3 <- flip runContT (pure . Grin.SReturn . Grin.Var) $ do
    e3 <-
      exprToGrin
        (\a -> lookup (name a) $ (\(b, c, _) -> (b, length c)) <$> defs3)
        name
        name
        e2
    res <- evalAs name (cInt . Grin.Var) (Grin.Var e3)
    yieldExp name $ Grin.SApp (Grin.packName "_prim_int_print") [Grin.Var res]
  let
    p = Grin.Program $
      fmap (\(a, b, c) -> Grin.Def a b c) defs3 <>
      [ Grin.Def (Grin.packName "grinMain") [] e3
      ]
  pure $ Grin.generateEval p

optimizeProgram :: Grin.Exp -> IO Grin.Exp
optimizeProgram e =
  Grin.optimize
    Grin.defaultOpts
    e
    []
    [Grin.PrintAST, Grin.SaveLLVM True "out"]
