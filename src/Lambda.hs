{-# language FlexibleContexts #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Lambda where

import Bound (Scope)
import Bound.Scope (fromScope, toScope, abstract)
import Bound.TH (makeBound)
import Bound.Var (Var(..), unvar)
import Control.Monad.State (MonadState, runState, get, gets, put)
import Control.Monad.Writer (MonadWriter, tell, runWriterT)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Int (Int64)
import Data.List (elemIndex, intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Void (Void, absurd)

import qualified Data.List.NonEmpty as NonEmpty
import qualified OrderedSet as Ordered

data Expr a
  = Var a
  | Call (Expr a) (NonEmpty (Expr a))
  | Lam !Int (Scope Int Expr a)
  | Int !Int64
  | Add (Expr a) (Expr a)
  deriving (Functor, Foldable, Traversable)
deriveShow1 ''Expr
deriveEq1 ''Expr
makeBound ''Expr
deriving instance Show a => Show (Expr a)
deriving instance Eq a => Eq (Expr a)

ppr
  :: forall a b m
   . MonadState [a] m
  => (a -> String)
  -> (b -> String)
  -> Expr b
  -> m String
ppr avar = go1
  where
    go1
      :: forall c
       . (c -> String)
      -> Expr c
      -> m String
    go1 bvar (Var a) = pure $ bvar a
    go1 _ (Int a) = pure $ show a
    go1 bvar (Add a b) = (\a' b' -> a' <> " + " <> b') <$> go3 bvar a <*> go2 bvar b
    go1 bvar (Call f xs) =
      (\f' xs' -> f' <> " " <> intercalate " " (NonEmpty.toList xs')) <$>
      go2 bvar f <*> traverse (go2 bvar) xs
    go1 bvar (Lam n s) = do
      ns <- do; (xs, xs') <- gets (splitAt n); fmap avar xs <$ put xs'
      (\s' -> "\\" <> intercalate " " ns <> " -> " <> s') <$>
        go1 (unvar (ns !!) bvar) (fromScope s)

    go3
      :: forall c
       . (c -> String)
      -> Expr c
      -> m String
    go3 bvar e@Var{} = go1 bvar e
    go3 bvar e@Int{} = go1 bvar e
    go3 bvar e@Add{} = (\x -> "(" <> x <> ")") <$> go1 bvar e
    go3 bvar e@Call{} = go2 bvar e
    go3 bvar e@Lam{} = go2 bvar e

    go2
      :: forall c
       . (c -> String)
      -> Expr c
      -> m String
    go2 bvar e@Var{} = go1 bvar e
    go2 bvar e@Int{} = go1 bvar e
    go2 bvar e@Add{} = go1 bvar e
    go2 bvar e@Call{} = (\x -> "(" <> x <> ")") <$> go1 bvar e
    go2 bvar e@Lam{} = (\x -> "(" <> x <> ")") <$> go1 bvar e

pprLifted :: MonadState [a] m => (a -> String) -> Lifted a -> m String
pprLifted var (Lifted a e) =
  (\e' -> var a <> " = " <> e') <$>
  ppr var absurd e

lam :: Eq a => [a] -> Expr a -> Expr a
lam as e =
  case e of
    Lam bs s ->
      Lam (l + bs) $
      toScope $
      unvar
        (B . (l+))
        (\a -> maybe (F a) B $ elemIndex a as) <$>
      fromScope s
    _ -> Lam l $ abstract (`elemIndex` as) e
  where
    l = length as

apply1 :: Monad f => f a -> Scope Int f a -> Scope Int f a
apply1 a =
  toScope .
  (>>= unvar (\n -> if n == 0 then F <$> a else pure $ B (n-1)) (pure . F)) .
  fromScope

-- | Close over a scope, returning the new scope, the number of variables abstracted,
-- and a list of the abstracted variables
closeScope
  :: forall f b
   . (Monad f, Traversable f, Eq b)
  => Scope Int f b
  -> (Scope Int f Void, Int, [b])
closeScope s = (res, l, Ordered.toList st)
  where
    l = Ordered.size st

    updateVar =
      unvar
        (pure . B . (+l))
        (\a -> do
          (n, st') <- gets $ Ordered.posInsertMay a
          case st' of
            Nothing -> pure $ B n
            Just st'' -> B n <$ put st'')

    (res, st) =
      runState
        (fmap toScope $ traverse updateVar $ fromScope s)
        Ordered.empty

data Lifted a = Lifted a (Expr Void)

nameSupply :: (IsString s, Semigroup s) => [s]
nameSupply = ("v" <>) . fromString . show <$> [1::Int ..]

liftLambdas :: (MonadState [a] m, Eq a) => Expr a -> m (Expr a, [Lifted a])
liftLambdas = runWriterT . liftLambdas' id
  where
    liftLambdas'
      :: forall a m
      . (MonadState [a] m, MonadWriter [Lifted a] m)
      => forall b. Eq b => (a -> b) -> Expr b -> m (Expr b)
    liftLambdas' ctx e =
      case e of
        Int n -> pure $ Int n
        Add a b -> Add <$> liftLambdas' ctx a <*> liftLambdas' ctx b
        Var a -> pure $ Var a
        Call f xs -> do
          f' <- liftLambdas' ctx f
          xs' <- traverse (liftLambdas' ctx) xs
          pure $ Call f' xs'
        Lam as s -> do
          s' <- liftLambdas'Scope ctx s
          n <- do; xs <- get; head xs <$ put (tail xs)
          case closeScope s' of
            (s'', lxs, xs) -> do
              tell [Lifted n $ Lam (as + lxs) s'']
              pure $ case Var <$> xs of
                [] -> Var $ ctx n
                v:vs -> Call (Var $ ctx n) $ v :| vs
      where
        liftLambdas'Scope :: forall b. Eq b => (a -> b) -> Scope Int Expr b -> m (Scope Int Expr b)
        liftLambdas'Scope ctx' = fmap toScope . liftLambdas' (F . ctx') . fromScope

fun1 :: Expr String
fun1 = lam ["x"] $ Call (Var "x") [lam ["y"] $ Call (Var "x") [Var "y"]]

example1 :: MonadState [String] m => m String
example1 = do
  (expr, defs) <- liftLambdas fun1
  (\a b c -> unlines $ "from\n" : a : "" : "to\n" : b : c) <$>
    ppr id id fun1 <*>
    ppr id id expr <*>
    traverse (pprLifted id) defs

y :: Expr String
y =
  lam ["f"] $
  Call
    (lam ["x"] $ Call (Var "f") [Call (Var "x") [Var "x"]])
    [lam ["x"] $ Call (Var "f") [Call (Var "x") [Var "x"]]]

example2 :: MonadState [String] m => m String
example2 = do
  (expr, defs) <- liftLambdas y
  (\a b c -> unlines $ "from\n" : a : "" : "to\n" : b : c) <$>
    ppr id id y <*>
    ppr id id expr <*>
    traverse (pprLifted id) defs

id' :: Expr String
id' = lam ["x"] $ Var "x"

succ' :: Expr String
succ' = lam ["x"] $ Add (Int 1) (Var "x")

tripleAdd' :: Expr String
tripleAdd' = lam ["x", "y", "z"] $ Add (Add (Var "x") (Var "y")) (Var "z")

compose' :: Expr String
compose' = lam ["f", "g", "x"] $ Call (Var "f") [Call (Var "g") [Var "x"]]