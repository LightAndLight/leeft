{-# language ScopedTypeVariables #-}
module Typecheck where

import Bound.Scope (Scope, fromScope)
import Bound.Var (unvar)
import Control.Lens.At (ix)
import Control.Lens.Fold ((^?))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))

import Type
import Lambda

checkScope
  :: Eq b
  => [Type b]
  -> (a -> Maybe (Type b))
  -> [NonEmpty (Type b)]
  -> Type b
  -> Scope Int Expr a
  -> Bool
checkScope bctx ctx args ty =
  check (unvar ((bctx ^?) . ix) ctx) args ty . fromScope

inferScope
  :: Eq b
  => [Type b]
  -> (a -> Maybe (Type b))
  -> [NonEmpty (Type b)]
  -> Scope Int Expr a
  -> Maybe (Type b)
inferScope bctx ctx args =
  infer (unvar ((bctx ^?) . ix) ctx) args . fromScope

check :: Eq b => (a -> Maybe (Type b)) -> [NonEmpty (Type b)] -> Type b -> Expr a -> Bool
check ctx [] ty (Lam n s) =
  case unrollN n ty of
    Nothing -> False
    Just (argTys, retTy) -> checkScope argTys ctx [] retTy s
check ctx args ty a = maybe False (== ty) $ infer ctx args a

infer :: Eq b => (a -> Maybe (Type b)) -> [NonEmpty (Type b)] -> Expr a -> Maybe (Type b)
infer ctx args (Lam _ s) =
  case args of
    [] -> Nothing
    as:args' -> do
      retTy <- inferScope (toList as) ctx args' s
      pure $ foldr TArr retTy as
infer ctx args (Var a) = ctx a
infer ctx args (Call f xs) = do
  xtys <- traverse (infer ctx args) xs
  fty <- infer ctx (xtys : args) f
  (xtys', retTy) <- unrollN (length xtys) fty
  if toList xtys == xtys'
    then Just retTy
    else Nothing
