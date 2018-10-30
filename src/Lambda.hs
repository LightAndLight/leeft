{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language StandaloneDeriving, TemplateHaskell #-}
{-# language ScopedTypeVariables #-}
module Lambda where

import Bound
import Bound.Scope
import Control.Monad.State (MonadState, get, put, evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (MonadWriter, tell, runWriter)
import Data.Bifunctor (first)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Foldable (toList)
import Data.List (elemIndex, intersect)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

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

-- | Abstracts over additional things in an 'Int'-indexed scope; 
abstractMore :: (Monad f, Foldable f, Eq a) => [a] -> Scope Int f a -> Scope Int f a
abstractMore vars s =
  toScope .
  (>>= \case
      B n -> pure $ B n
      F x -> maybe (pure $ F x) (pure . B) (x `elemIndex` vars)) .
  fromScope .
  mapBound (+numVars) $
  s
  where
    numVars = length $ intersect (toList s) vars

abstracted'
  :: ( MonadState [a] m, MonadWriter [(a, Expr' a)] m
     , Eq a
     )
  => Expr' a -> m (Expr' a)
abstracted' (Var' a) = pure $ Var' a
abstracted' (Call' f args) = Call' <$> abstracted' f <*> traverse abstracted' args
abstracted' (Lam' s) = _
  where
    go
      :: ( MonadState [a] m, MonadWriter [(a, Expr' a)] m
         , Eq a
         )
      => Scope Int Expr' a -> m (Expr' a)
    go s =
      case fromScope s of
        Var' (B n) -> pure $ Lam' (Scope (Var' (B n)))
        Var' (F x) -> pure $ Call' (Lam' (Scope (Var' (B 0)))) (Var' x :| [])
        Lam' x -> _
        Call' x y -> _

abstracted
  :: ( MonadState [a] m, MonadWriter [(a, Expr' a)] m
     , Eq a
     )
  => Expr' a -> m (Expr' a)
abstracted (Var' a) = pure $ Var' a
abstracted (Call' f args) = Call' <$> abstracted f <*> traverse abstracted args
abstracted (Lam' s) = do
  a:as <- get; put as
  let
    vars = toList s
    numVars = length vars
  case vars of
    [] -> do
      tell [(a, Lam' s)]
      pure $ Var' a
    v:vs -> do
      tell [(a, Lam' $ abstractMore vars s)]
      pure $ Call' (Var' a) (fmap pure $ v :| vs)

liftLambdas :: Expr String -> (Expr' String, [(String, Expr' String)])
liftLambdas tm = runWriter (evalStateT (abstracted $ merge tm) $ ("name"++) . show <$> [1..])

test :: (Expr' String, [(String, Expr' String)])
test = liftLambdas tm
  where
    tm = lam "x" $ App (Var "x") (Var "y")

test2 :: (Expr' String, [(String, Expr' String)])
test2 = liftLambdas tm
  where
    tm = lam "x" $ App (Var "x") (lam "y" $ App (Var "x") (Var "z"))