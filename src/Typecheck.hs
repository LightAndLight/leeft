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
  :: [Type]
  -> (a -> Maybe Type)
  -> [NonEmpty Type]
  -> Type
  -> Scope Int Expr a
  -> Bool
checkScope bctx ctx args ty =
  check (unvar ((bctx ^?) . ix) ctx) args ty . fromScope

inferScope
  :: [Type]
  -> (a -> Maybe Type)
  -> [NonEmpty Type]
  -> Scope Int Expr a
  -> Maybe Type
inferScope bctx ctx args =
  infer (unvar ((bctx ^?) . ix) ctx) args . fromScope

check :: (a -> Maybe Type) -> [NonEmpty Type] -> Type -> Expr a -> Bool
check ctx [] ty (Lam n s) =
  case unrollN n ty of
    Nothing -> False
    Just (argTys, retTy) -> checkScope argTys ctx [] retTy s
check ctx args ty a = maybe False (== ty) $ infer ctx args a

infer :: (b -> Maybe Type) -> [NonEmpty Type] -> Expr b -> Maybe Type
infer ctx args (Lam _ s) =
  case args of
    [] -> Nothing
    as:args' -> do
      retTy <- inferScope (toList as) ctx args' s
      pure $ foldr Arr retTy as
infer ctx args (Var a) = ctx a
infer ctx args (Call f xs) = do
  xtys <- traverse (infer ctx args) xs
  fty <- infer ctx (xtys : args) f
  (xtys', retTy) <- unrollN (length xtys) fty
  if toList xtys == xtys'
    then Just retTy
    else Nothing
