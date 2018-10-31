{-# language EmptyCase #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language StandaloneDeriving, TemplateHaskell #-}
{-# language ScopedTypeVariables #-}
{-# language ExistentialQuantification #-}
{-# language DataKinds, GADTs, KindSignatures #-}
module Lambda where

import Bound
import Bound.Scope
import Control.Monad (ap)
import Control.Monad.State (MonadState, get, put, evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (MonadWriter, tell, runWriter, runWriterT)
import Data.Bifunctor (first)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Foldable (toList)
import Data.List (elemIndex, intersect, nub)
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

-- | Abstract over a single variable. If the target is already a lambda,
-- the abstracted variable is now the 0-th argument, and the other arguments
-- are shifted up
abstract1' :: Eq a => a -> Expr' a -> Expr' a
abstract1' a e =
  case e of
    Lam' s ->
      Lam' . Scope .
      (>>= \case
          B n -> pure (B $ n+1)
          F f -> unscope $ abstract0 a f) .
      unscope $
      s
    _ -> Lam' $ abstract0 a e
  where
    abstract0 b = abstract (\x -> if x == b then Just 0 else Nothing)

data Nat = Z | S Nat
data Fin :: Nat -> * where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)
deriving instance Show (Fin n)

data SNat :: Nat -> * where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)
deriving instance Show (SNat n)

data Expr'' a
  = Var'' a
  | Call'' (Expr'' a) (NonEmpty (Expr'' a))
  | forall n. Lam'' (SNat n) (Scope (Fin n) Expr'' a)
deriving instance Functor Expr''
deriving instance Foldable Expr''
deriving instance Traversable Expr''
deriveShow1 ''Expr''
deriving instance Show a => Show (Expr'' a)

instance Applicative Expr'' where; pure = return; (<*>) = ap

instance Monad Expr'' where
  return = Var''
  Var'' a >>= f = f a
  Call'' a b >>= f = Call'' (a >>= f) (fmap (>>= f) b)
  Lam'' n s >>= f = Lam'' n (s >>>= f)

fzScope_from :: Scope (Fin 'Z) Expr'' a -> Expr'' a
fzScope_from = instantiateEither (either fzAbsurd Var'')
  where
    fzAbsurd :: Fin 'Z -> a
    fzAbsurd a = case a of

fzScope_to :: Expr'' a -> Scope (Fin 'Z) Expr'' a
fzScope_to = lift

abstract1''
  :: (Eq a, Monad f)
  => a
  -> Scope (Fin n) f a
  -> Scope (Fin ('S n)) f a
abstract1'' a (Scope s) =
  Scope $
  s >>= \e -> case e of
    B n -> pure $ B (FS n)
    F x -> do
      x' <- x
      pure $
        if x' == a
        then B FZ
        else F x

instantiate1' :: Monad f => f a -> Scope (Fin ('S n)) f a -> Scope (Fin n) f a
instantiate1' e (Scope s) =
  Scope $
  s >>= \a ->
  pure $ case a of
    B FZ -> F e
    B (FS n) -> B n
    F x -> F x

lam' :: Eq a => a -> Expr'' a -> Expr'' a
lam' a (Lam'' n s) = Lam'' (SS n) (abstract1'' a s)
lam' a e = Lam'' (SS SZ) $ abstract1'' a $ lift e

{-

if I have an (Expr'' a), then I have a closed term with free variables at the leaves

if I have a (Expr'' (Var (Fin n) a)) then I have an open term - there are bound
and free variables at the leaves. I want to abstract over the available free
variables

-}

data E (s :: * -> (* -> *) -> * -> *) f a = forall n. E (s (Fin n) f a)

liftLambdas
  :: ( MonadState [a] m, MonadWriter [(a, Expr'' a)] m
     , Eq a
     )
  => Expr'' a -> m (Expr'' a)
liftLambdas (Var'' a) = pure $ Var'' a
liftLambdas (Call'' f xs) = do
  res <- Call'' <$> liftLambdas f <*> traverse liftLambdas xs
  let frees = nub $ toList res
  pure $ case Var'' <$> frees of
    [] -> res
    v:vs -> Call'' (foldr lam' res frees) $ v:|vs
liftLambdas (Lam'' n s) = do
  case n of
    SZ -> liftLambdas (fzScope_from s)
    SS k ->
      _ (unscope s)


{-
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

liftLambdas' :: Expr String -> (Expr' String, [(String, Expr' String)])
liftLambdas' tm = runWriter (evalStateT (abstracted $ merge tm) $ ("name"++) . show <$> [1..])

test :: (Expr' String, [(String, Expr' String)])
test = liftLambdas' tm
  where
    tm = lam "x" $ App (Var "x") (Var "y")

test2 :: (Expr' String, [(String, Expr' String)])
test2 = liftLambdas' tm
  where
    tm = lam "x" $ App (Var "x") (lam "y" $ App (Var "x") (Var "z"))

-}
