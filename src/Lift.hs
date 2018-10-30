{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving, TemplateHaskell #-}
module Lift where

import Bound (Scope, makeBound)
import Bound.Scope
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (MonadWriter, Writer, tell)
import Data.Deriving (deriveShow1)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void, absurd)

import qualified Lambda

data Expr a
  = Var !a
  | App (Expr a) (Expr a)
  deriving (Functor, Foldable, Traversable)
deriveShow1 ''Expr
makeBound ''Expr
deriving instance Show a => Show (Expr a)

newtype Arg = Arg Int deriving (Num, Eq, Show)
newtype Name = Name Int deriving (Num, Eq, Show)

data Binding a = Binding { unBinding :: Scope (Either Arg Name) Expr a }
  deriving (Functor, Foldable, Traversable)
deriveShow1 ''Binding
deriving instance Show a => Show (Binding a)

liftExpr :: Lambda.Expr s -> [Binding s]
liftExpr e =
  case e of
    Lambda.Var a -> [Binding $ lift (Var a)]
    Lambda.Lam{} -> _
    Lambda.App f x -> _