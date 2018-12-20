{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language KindSignatures, TypeFamilies #-}
module Defun where

import Bound.Scope (Scope, toScope, fromScope)
import Bound.TH (makeBound)
import Bound.Var (Var(..), unvar)
import Control.Lens.Plated (Plated(..), gplate)
import Control.Monad.Trans (lift)
import Data.Int (Int64)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics (Generic)
import Text.PrettyPrint.ANSI.Leijen (Pretty(..), Doc)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.PrettyPrint.ANSI.Leijen as Print

import Fresh.Class (MonadFresh)
import qualified Lambda as Lambda

data Defun a b
  = Var b
  | Global a
  | Call a (NonEmpty (Defun a b))
  | App (Defun a b) (NonEmpty (Defun a b))
  | Int !Int64
  | Add (Defun a b) (Defun a b)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeBound ''Defun
makeBaseFunctor ''Defun

instance Plated (Defun a b) where; plate = gplate

data Program a = Program [Def a] (Defun a a)
data Def a = Def a !Int (Scope Int (Defun a) a)

defun
  :: (Eq a, MonadFresh s a m)
  => Lambda.Program Lambda.Def a
  -> m (Program a)
defun e = do
  Lambda.Program ds e' <- Lambda.liftLambdas e
  pure $ Program (defunLifted <$> ds) (defunExprTop id e')

defunLifted
  :: Lambda.Lifted a
  -> Def a
defunLifted (Lambda.Lifted a def) = uncurry (Def a) $ defunExpr id def

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

--- pretty printing

prettyDefun :: forall a b. (a -> Doc) -> (b -> Doc) -> Defun a b -> Doc
prettyDefun aDoc bDoc = go1
  where
    go1 :: Defun a b -> Doc
    go1 (Var a) = bDoc a
    go1 (Global a) = aDoc a
    go1 (Int a) = Print.text $ show a
    go1 (Add a b) =
      Print.hsep [go3 a, Print.char '+', go2 b]
    go1 (Call f xs) =
      Print.hsep $ aDoc f : NonEmpty.toList (go2 <$> xs)
    go1 (App f xs) =
      Print.hsep $ go2 f : NonEmpty.toList (go2 <$> xs)

    go2 :: Defun a b -> Doc
    go2 e@Var{} = go1 e
    go2 e@Global{} = go1 e
    go2 e@Int{} = go1 e
    go2 e@Add{} = go1 e
    go2 e@Call{} = Print.parens $ go1 e
    go2 e@App{} = Print.parens $ go1 e

    go3 :: Defun a b -> Doc
    go3 e@Var{} = go1 e
    go3 e@Global{} = go1 e
    go3 e@Int{} = go1 e
    go3 e@Add{} = Print.parens $ go1 e
    go3 e@Call{} = go2 e
    go3 e@App{} = go2 e

prettyProgram
  :: (a -> Doc)
  -> Program a
  -> Doc
prettyProgram aDoc (Program a b) =
  Print.vsep $
  fmap (prettyDef aDoc) a <>
  [Print.hsep [Print.text "main", Print.char '=', prettyDefun aDoc aDoc b]]

prettyDef :: (a -> Doc) -> Def a -> Doc
prettyDef aDoc (Def a n b) =
  Print.hsep
  [ aDoc a
  , Print.char '='
  , Print.char lambda <>
    Print.braces (Print.hsep $ Print.int <$> [0..n-1]) <>
    Print.dot
  , prettyDefun aDoc (unvar Print.int aDoc) (fromScope b)
  ]
  where
    lambda = 'λ'

instance (Pretty a, Pretty b) => Pretty (Defun a b) where
  pretty = prettyDefun pretty pretty

instance Pretty a => Pretty (Def a) where
  pretty = prettyDef pretty

instance Pretty a => Pretty (Program a) where
  pretty = prettyProgram pretty
