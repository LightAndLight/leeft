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
import Data.Bifunctor (bimap)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Foldable (toList)
import Data.List (elemIndex, nub)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import Data.Void (Void)

data Expr a
  = Var a
  | App (Expr a) (Expr a)
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
      let
        l = length as
      in
        Lam (l + bs) $ toScope $ fmap (unvar (B . (l+)) (\a -> maybe (F a) B $ elemIndex a as)) $ fromScope s
    _ -> Lam (length as) $ abstract (`elemIndex` as) e

-- | If a term is closed then it can be cast to other types
cast :: Expr a -> Maybe (Expr b)
cast = traverse (const Nothing)

replace :: a -> Expr a -> (Bool, Expr a)
replace val e =
  case e of
    Var a -> (False, Var a)
    App f x -> do
      case replace val f of
        (True, f') -> (True, App f' x)
        (False, _) -> App f <$> replace val x
    Lam as s ->
      case replace' val s of
        (True, s') -> (True, Lam as s')
        (False, _) -> (True, Var val)
  where
    replace' :: b -> Scope Int Expr b -> (Bool, Scope Int Expr b)
    replace' b = fmap toScope . replace (F b) . fromScope

replace2 :: Eq a => a -> Expr a -> (Maybe (Expr a), Expr a)
replace2 val e =
  case e of
    Var a -> (Nothing, Var a)
    App f x -> do
      case replace2 val f of
        (Just e, f') -> (Just e, App f' x)
        (Nothing, _) -> App f <$> replace2 val x
    Lam as s ->
      case replace2' val s of
        (Just e, s') -> (Just e, Lam as s')
        (Nothing, _) ->
          let
            frees = nub $ toList e
          in
            (cast $ lam frees e, Var val)
  where
    replace2' :: Eq b => b -> Scope Int Expr b -> (Maybe (Expr b), Scope Int Expr b)
    replace2' b = bimap (>>= cast) toScope . replace2 (F b) . fromScope

cdr :: (a, b, c) -> (b, c)
cdr (_, b, c) = (b, c)

replace3 :: Eq a => [a] -> Expr a -> ([a], [(a, Expr a)], Expr a)
replace3 supply e =
  case e of
    Var a -> (supply, [], Var a)
    App f x ->
      let
        (supply', es, f') = replace3 supply f
        (supply'', es', x') = replace3 supply' x
      in
        (supply'', es ++ es', App f' x')
    Lam as s ->
      case replace3' supply s of
        (supply', es, s') ->
          case supply' of
            n:supply'' ->
              let
                frees = nub $ toList s'
              in
                (supply'', (n, lam frees e):es, foldr (\x f -> App f (Var x)) (Var n) frees)
  where
    replace3' :: Eq b => [b] -> Scope Int Expr b -> ([b], [(b, Expr b)], Scope Int Expr b)
    replace3' supply =
      (\(x, y, z) -> (unvar undefined id <$> x, bimap (unvar undefined id) (fromJust . cast) <$> y, toScope z)) .
      replace3 (F <$> supply) .
      fromScope

{-
liftLambdasScope
  :: (Eq b, MonadState [b] m, MonadWriter [(b, Int, Scope Int Expr b)] m)
  => (Int -> b) -> (a -> b) -> Scope Int Expr a -> m (Expr b)
liftLambdasScope g f s = liftLambdas (unvar g f) $ fromScope s

{-

> lam ["x"] (App (lam ["x"] $ Var "y") (Var "x"))
Lam 1 (Scope (App (Lam 1 (Scope (Var (F (Var (F (Var "y"))))))) (Var (B 0)))) :: Expr String
Scope (App (Lam 1 (Scope (Var (F (Var (F (Var "y"))))))) (Var (B 0))) :: Scope Int Expr String
App (Lam 1 (Scope (Var (F (Var (F "y")))))) (Var (B 0)) :: Expr (Var Int String)
Lam 1 (Scope (Var (F (Var (F "y")))))  :: Expr (Var Int String)
Scope (Var (F (Var (F "y"))))  :: Scope Int Expr (Var Int String)
Var (F (F "y"))  :: Expr (Var Int (Var Int String))

-}

liftLambdas''
  :: (Eq b, MonadState [b] m, MonadWriter [(b, Int, Scope Int Expr b)] m)
  => (a -> b) -> Expr (Var Int a) -> m (Expr (Var Int b))
liftLambdas'' f e =
  case e of
    Var (F x) -> _
    Var (B x) -> _
    App a b -> _
    Lam as s -> _

liftLambdas
  :: (Eq b, MonadState [b] m, MonadWriter [(b, Int, Scope Int Expr b)] m)
  => (a -> b) -> Expr a -> m (Expr b)
liftLambdas f e =
  case e of
    Var a -> _
    App a b -> App <$> liftLambdas f a <*> liftLambdas f b
    Lam as s -> _
-}