{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language StandaloneDeriving, TemplateHaskell #-}
module Lambda where

import Bound
import Bound.Scope
import Control.Monad.State (MonadState, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Bifunctor (first)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Foldable (toList)
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Void (Void)

data Expr a
  = Var a
  | App (Expr a) (Expr a)
  | Lam (Scope () Expr a)
  deriving (Functor, Foldable, Traversable)
deriveShow1 ''Expr
deriveEq1 ''Expr
makeBound ''Expr
deriving instance Show a => Show (Expr a)
deriving instance Eq a => Eq (Expr a)

lam :: Eq a => a -> Expr a -> Expr a
lam x e = Lam $ abstract1 x e

eval :: Expr a -> Expr a
eval (App f x) =
  case eval f of
    Lam s -> instantiate1 x s
    f' -> App f' x
eval a = a

data Expr' a
  = Var' a
  | Call' (Expr' a) (NonEmpty (Expr' a))
  | Lam' (Scope Int Expr' a)
  deriving (Functor, Foldable, Traversable)
deriveShow1 ''Expr'
makeBound ''Expr'
deriving instance Show a => Show (Expr' a)

-- | Collapse nested lambdas
mergeLams :: Scope () Expr a -> Scope Int Expr a
mergeLams = go 0
  where
    go :: Int -> Scope () Expr a -> Scope Int Expr a
    go n s =
      case unscope s of
        Var (B _) -> Scope $ Var (B n)
        Var (F a) -> lift a
        Lam a ->
          go (n+1) a >>=
          \case
            B _ -> Scope $ Var (B n)
            F c -> lift c
        App a b -> Scope $ first (const n) <$> App a b

-- | Groups sequential function applications
merge :: Expr a -> Expr' a
merge (Var a) = Var' a
merge (Lam s) = Lam' (hoistScope merge $ mergeLams s)
merge (App f x) =
  case merge f of
    Call' f' args -> Call' f' $ args <> pure (merge x)
    f' -> Call' f' $ pure (merge x)

-- | Abstracts over any free variables in a term and re-applies them
abstracted :: Eq a => Expr' a -> Expr' a
abstracted (Var' a) = Var' a
abstracted e =
  let
    vars = toList e
  in
    case vars of
      [] -> e
      v:vs -> Call' (Lam' $ abstract (`elemIndex` vars) e) (fmap pure $ v :| vs)

collect
  :: (MonadState [a] m, MonadWriter [(a, Scope Int Expr' a)] m)
  => Expr' a -> m (Expr' a)
collect (Var' a) = pure $ Var' a
collect (Call' f args) = Call' <$> collect f <*> traverse collect args
collect (Lam' s) = do
  a:as <- get; put as
  tell [(a, s)]
  collect $ _ s
  pure $ Var' a