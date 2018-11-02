module Bound.Util where

import Bound.Scope (Scope, toScope, fromScope)
import Bound.Var (Var(..), unvar)
import Control.Lens.At (ix)
import Control.Lens.Fold ((^?))
import Data.Maybe (fromMaybe)

-- | Apply a single argument to a multi-argument function
--
-- Assumes that the scope will have at least 1 bound variable left over
apply1 :: Monad f => f a -> Scope Int f a -> Scope Int f a
apply1 a =
  toScope .
  (>>= unvar (\n -> if n == 0 then F <$> a else pure $ B (n-1)) (pure . F)) .
  fromScope

-- | Apply a some arguments to a multi-argument function
--
-- Assumes that the scope will have at least 1 bound variable left over
applySome :: Monad f => [f a] -> Scope Int f a -> Scope Int f a
applySome as =
  toScope .
  (>>= unvar (\n -> maybe (pure . B $ n-l) (fmap F) $ as ^? ix n) (pure . F)) .
  fromScope
  where
    l = length as
