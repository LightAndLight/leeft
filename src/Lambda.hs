{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Lambda where

import Bound (Scope)
import Bound.Scope (fromScope, toScope, abstract)
import Bound.TH (makeBound)
import Bound.Var (Var(..), unvar)
import Control.Lens.TH (makePrisms)
import Control.Monad.State (MonadState, runState, get, gets, put)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void)

import qualified OrderedSet as Ordered

data Expr a
  = Var a
  | Call (Expr a) (NonEmpty (Expr a))
  | Lam Int (Scope Int Expr a)
  deriving (Functor, Foldable, Traversable)
deriveShow1 ''Expr
deriveEq1 ''Expr
makeBound ''Expr
deriving instance Show a => Show (Expr a)
deriving instance Eq a => Eq (Expr a)

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
          (n, st) <- gets $ Ordered.posInsertMay a
          case st of
            Nothing -> pure $ B n
            Just st' -> B n <$ put st')

    (res, st) =
      runState
        (fmap toScope $ traverse updateVar $ fromScope s)
        Ordered.empty

liftLambdas
  :: forall a m
   . (MonadState [a] m, MonadWriter [(a, Expr Void)] m)
  => forall b. Eq b => (a -> b) -> Expr b -> m (Expr b)
liftLambdas ctx e =
  case e of
    Var a -> pure $ Var a
    Call f xs -> do
      f' <- liftLambdas ctx f
      xs' <- traverse (liftLambdas ctx) xs
      pure $ Call f' xs'
    Lam as s -> do
      s' <- liftLambdasScope ctx s
      n <- do; x:xs <- get; x <$ put xs
      case closeScope s' of
        (s'', lxs, xs) -> do
          tell [(n, Lam (as + lxs) s'')]
          pure $ case Var <$> xs of
            [] -> Var $ ctx n
            v:vs -> Call (Var $ ctx n) $ v :| vs
  where
    liftLambdasScope :: forall b. Eq b => (a -> b) -> Scope Int Expr b -> m (Scope Int Expr b)
    liftLambdasScope ctx = fmap toScope . liftLambdas (F . ctx) . fromScope