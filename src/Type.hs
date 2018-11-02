{-# language BangPatterns #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language StandaloneDeriving, TemplateHaskell #-}
module Type where

import Bound.Scope (Scope)
import Bound.TH (makeBound)
import Control.Lens.TH (makePrisms)
import Data.Bifunctor (first)
import Data.Deriving (deriveEq1, deriveShow1, deriveOrd1)

data Type a
  = TUnit
  | TVar a
  | TArr (Type a) (Type a)
  | TForall Int (Scope Int Type a)
  deriving (Functor, Foldable, Traversable)
deriveEq1 ''Type
deriveShow1 ''Type
deriveOrd1 ''Type
deriving instance Eq a => Eq (Type a)
deriving instance Show a => Show (Type a)
deriving instance Ord a => Ord (Type a)
makeBound ''Type
makePrisms ''Type

-- | Get the first n "argument" types of a function, and then the rest
unrollN :: Int -> Type a -> Maybe ([Type a], Type a)
unrollN !0 t = Just ([], t)
unrollN !n (TArr t ts) = first (t:) <$> unrollN (n-1) ts
unrollN !_ _ = Nothing
