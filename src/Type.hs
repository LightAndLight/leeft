{-# language BangPatterns #-}
module Type where

import Data.Bifunctor (first)

data Type
  = Unit
  | Arr Type Type
  deriving (Eq, Show)

-- | Get the first n "argument" types of a function, and then the rest
unrollN :: Int -> Type -> Maybe ([Type], Type)
unrollN !0 t = Just ([], t)
unrollN !n (Arr t ts) = first (t:) <$> unrollN (n-1) ts
unrollN !_ _ = Nothing

-- | Split a sequence of function arrows into "argument types" and
-- "return type"
unroll :: Type -> ([Type], Type)
unroll (Arr t ts) = first (t:) $ unroll ts
unroll t = ([], t)
