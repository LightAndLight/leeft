{-# language GeneralizedNewtypeDeriving #-}
module Lift where

import Bound (Scope)
import Bound.Scope (abstractEither, splat)
import Control.Monad.Writer (Writer)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void, absurd)

import qualified Lambda

data Expr a
  = Var !a
  | Call String (Expr a)

data Params a
  = Params (Scope Int Expr a)
  | Expr (Expr a)

data Binding a = Binding a (Params a)

newtype LL a = LL { unLL :: Writer [Binding String] a }
  deriving (Functor, Applicative, Monad)

liftBinding :: Lambda.Binding s -> LL (Binding s)
liftBinding (Lambda.Binding n e) = _

liftParams :: Lambda.Expr s -> LL (Params s)
liftParams = _

liftExpr :: Lambda.Expr s -> LL (Expr s)
liftExpr = _
