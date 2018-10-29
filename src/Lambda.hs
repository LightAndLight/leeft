{-# language RankNTypes #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language StandaloneDeriving, TemplateHaskell #-}
module Lambda where

import Bound
import Data.Deriving (deriveShow1)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Void (Void)

data Expr a
  = Var !a
  | App (Expr a) (Expr a)
  | Abs (Scope () Expr a)
  deriving (Functor, Foldable, Traversable)

deriveShow1 ''Expr
makeBound ''Expr

deriving instance Show a => Show (Expr a)

data Binding a = Binding a (Expr a)
  deriving Show

collectApps :: Expr a -> Either (Expr a) (Expr a, NonEmpty (Expr a))
collectApps (App f x) =
  case collectApps f of
    Left a -> Right (a, pure x)
    Right (f', args) -> Right (f', args <> pure x)
collectApps a = Left a
