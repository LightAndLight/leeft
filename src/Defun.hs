{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language TemplateHaskell #-}
module Defun where

import Bound.Scope (Scope, toScope, fromScope)
import Bound.TH (makeBound)
import Bound.Var (Var(..), unvar)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (lift)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void, absurd)
import qualified Lambda as Lambda

data Defun a b
  = Var b
  | Global a
  | Call a (NonEmpty (Defun a b))
  | App (Defun a b) (NonEmpty (Defun a b))
  | Int !Int64
  | Add (Defun a b) (Defun a b)
  deriving (Eq, Show, Functor, Foldable, Traversable)
makeBound ''Defun

data Def a = Def a !Int (Scope Int (Defun a) Void)

defun
  :: (Eq a, MonadState [a] m)
  => Lambda.Expr a
  -> m (Defun a a, [Def a])
defun e = do
  (e', ds) <- Lambda.liftLambdas e
  pure (defunExprTop id e', defunLifted <$> ds)

defunLifted
  :: Lambda.Lifted a
  -> Def a
defunLifted (Lambda.Lifted a def) = uncurry (Def a) $ defunExpr absurd def

defunExprTop
  :: (b -> a)
  -> Lambda.Expr b
  -> Defun a b
defunExprTop _ (Lambda.Lam _ _) = error "lambdas should have been eliminated"
defunExprTop ctx a = defunExprInner (Global . ctx) a

defunExpr
  :: (b -> a)
  -> Lambda.Expr b
  -> (Int, Scope Int (Defun a) b)
defunExpr ctx (Lambda.Lam n s) =
  ( n
  , toScope $
    defunExprInner (unvar (Var . B) (Global . ctx)) $
    fromScope s
  )
defunExpr ctx a = (0, lift $ defunExprInner (Global . ctx) a)

defunExprInner
  :: (b -> Defun a b)
  -> Lambda.Expr b
  -> Defun a b
defunExprInner ctx e =
  case e of
    Lambda.Var a -> ctx a
    Lambda.Call (Lambda.Var f) xs ->
      case ctx f of
        Global f' -> Call f' (defunExprInner ctx <$> xs)
        f' -> App f' (defunExprInner ctx <$> xs)
    Lambda.Call f xs ->
      case defunExprInner ctx f of
        Call f' xs' -> Call f' (xs' <> fmap (defunExprInner ctx) xs)
        f' -> App f' (defunExprInner ctx <$> xs)
    Lambda.Lam _ _ -> error "nested lambdas should have been eliminated"
    Lambda.Int n -> Int n
    Lambda.Add a b -> Add (defunExprInner ctx a) (defunExprInner ctx b)