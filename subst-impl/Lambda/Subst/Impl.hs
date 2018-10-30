module Lambda.Subst.Impl where

import Control.Lens.Prism (Prism')

import Lambda.Expr

type Term = Expr
type F = String

_B :: Prism' Expr Int
_B = _Bound

_F :: Prism' Expr String
_F = _Free

_Binder :: Prism' Expr Expr
_Binder = _Lam
