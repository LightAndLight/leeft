{-# language FlexibleContexts #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language StandaloneDeriving, TemplateHaskell #-}
module Lambda where

import Bound
import Bound.Scope
import Bound.Var
import Control.Monad (replicateM)
import Control.Monad.State (MonadState, get, put, evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (MonadWriter, tell, runWriter, runWriterT)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Foldable (toList)
import Data.List (elemIndex, nub)
import Data.List.NonEmpty (NonEmpty(..))

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
lam as = Lam (length as) . abstract (`elemIndex` as)

liftLambdasScope
  :: (Eq b, MonadState [b] m, MonadWriter [(b, Int, Scope Int Expr b)] m)
  => (a -> b) -> Scope Int Expr a -> m (Scope Int Expr b)
liftLambdasScope f s = _ (fromScope s)

liftLambdas
  :: (Eq b, MonadState [b] m, MonadWriter [(b, Int, Scope Int Expr b)] m)
  => (a -> b) -> Expr a -> m (Expr b)
liftLambdas f e =
  case e of
    Var a -> pure . Var $ f a
    Call a bs -> do
      res <- Call <$> liftLambdas f a <*> traverse (liftLambdas f) bs
      let frees = nub $ toList res
      pure $ case Var <$> frees of
        [] -> res
        x:xs -> Call (Lam (length frees) $ abstract (`elemIndex` frees) res) (x:|xs)
    Lam as s -> do
      n:ns <- get; put ns
      res <- Lam as <$> liftLambdasScope f s
      let
        frees = nub $ toList res
        res' = abstract (`elemIndex` frees) res
      tell [(n, length frees, res')]
      pure $ case Var <$> frees of
        [] -> res
        x:xs -> Call (Var n) (x:|xs)
