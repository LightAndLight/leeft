{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Lambda where

import Bound (Scope)
import Bound.Scope (fromScope, toScope, abstract)
import Bound.TH (makeBound)
import Bound.Var (Var(..), unvar)
import Control.Monad.State (runState, gets, put)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter, tell, runWriterT)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Int (Int64)
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Void (Void, absurd)
import Text.PrettyPrint.ANSI.Leijen (Pretty(..), Doc)

import qualified Text.PrettyPrint.ANSI.Leijen as Print

import Fresh.Class (MonadFresh, fresh, Stream(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified OrderedSet as Ordered

data Expr a
  = Var a
  | Call (Expr a) (NonEmpty (Expr a))
  | Lam !Int (Scope Int Expr a)
  | Int !Int64
  | Add (Expr a) (Expr a)
  deriving (Functor, Foldable, Traversable)
deriveShow1 ''Expr
deriveEq1 ''Expr
makeBound ''Expr
deriving instance Show a => Show (Expr a)
deriving instance Eq a => Eq (Expr a)

lam :: Eq a => [a] -> Expr a -> Expr a
lam as e =
  case e of
    Lam bs s ->
      Lam (l + bs) $
      toScope $
      unvar
        (B . (l+))
        (\a -> maybe (F a) B $ elemIndex a as) <$>
      fromScope s
    _ -> Lam l $ abstract (`elemIndex` as) e
  where
    l = length as

apply1 :: Monad f => f a -> Scope Int f a -> Scope Int f a
apply1 a =
  toScope .
  (>>= unvar (\n -> if n == 0 then F <$> a else pure $ B (n-1)) (pure . F)) .
  fromScope

closeExpr
  :: forall b
   . Eq b
  => Expr b
  -> (Scope Int Expr Void, Int, [b])
closeExpr = closeScope . lift

-- | Close over a scope, returning the new scope, the number of variables abstracted,
-- and a list of the abstracted variables
closeScope
  :: forall f b
   . (Monad f, Traversable f, Eq b)
  => Scope Int f b
  -> (Scope Int f Void, Int, [b])
closeScope s = (res, l, Ordered.toList st)
  where
    l = Ordered.size st

    updateVar =
      unvar
        (pure . B . (+l))
        (\a -> do
          (n, st') <- gets $ Ordered.posInsertMay a
          case st' of
            Nothing -> pure $ B n
            Just st'' -> B n <$ put st'')

    (res, st) =
      runState
        (fmap toScope $ traverse updateVar $ fromScope s)
        Ordered.empty

data Lifted a = Lifted a (Expr Void)

nameSupply :: forall s. (IsString s, Semigroup s) => Stream s
nameSupply = go 0
  where
    go :: Int -> Stream s
    go !n = Cons ("v" <> fromString (show n)) (go $ n+1)

liftLambdas :: (MonadFresh s a m, Eq a) => Expr a -> m (Expr a, [Lifted a])
liftLambdas = runWriterT . liftLambdas' id
  where
    liftLambdas'
      :: forall s a m
      . (MonadFresh s a m, MonadWriter [Lifted a] m)
      => forall b. Eq b => (a -> b) -> Expr b -> m (Expr b)
    liftLambdas' ctx e =
      case e of
        Int n -> pure $ Int n
        Add a b -> Add <$> liftLambdas' ctx a <*> liftLambdas' ctx b
        Var a -> pure $ Var a
        Call f xs -> do
          f' <- liftLambdas' ctx f
          xs' <- traverse (liftLambdas' ctx) xs
          pure $ Call f' xs'
        Lam as s -> do
          s' <- liftLambdas'Scope ctx s
          n <- fresh
          case closeScope s' of
            (s'', lxs, xs) -> do
              tell [Lifted n $ Lam (as + lxs) s'']
              pure $ case Var <$> xs of
                [] -> Var $ ctx n
                v:vs -> Call (Var $ ctx n) $ v :| vs
      where
        liftLambdas'Scope
          :: forall b
           . Eq b
          => (a -> b)
          -> Scope Int Expr b
          -> m (Scope Int Expr b)
        liftLambdas'Scope ctx' =
          fmap toScope . liftLambdas' (F . ctx') . fromScope

fun1 :: Expr String
fun1 = lam ["x"] $ Call (Var "x") [lam ["y"] $ Call (Var "x") [Var "y"]]

y :: Expr String
y =
  lam ["f"] $
  Call
    (lam ["x"] $ Call (Var "f") [Call (Var "x") [Var "x"]])
    [lam ["x"] $ Call (Var "f") [Call (Var "x") [Var "x"]]]

id' :: Expr String
id' = lam ["x"] $ Var "x"

succ' :: Expr String
succ' = lam ["x"] $ Add (Int 1) (Var "x")

const' :: Expr String
const' = lam ["x"] $ lam ["y"] $ Var "x"

tripleAdd' :: Expr String
tripleAdd' = lam ["x", "y", "z"] $ Add (Add (Var "x") (Var "y")) (Var "z")

compose' :: Expr String
compose' = lam ["f", "g", "x"] $ Call (Var "f") [Call (Var "g") [Var "x"]]


--- Pretty printing


subZero :: Char
subZero = '₀'

subDigits :: [Char]
subDigits = take 10 $ iterate succ subZero

showSub :: Int -> String
showSub = fmap ((subDigits !!) . read . pure) . show

lambda :: Char
lambda = 'λ'

prettyExpr :: (a -> Doc) -> Expr a -> Doc
prettyExpr = go1 0
  where
    go1 :: Int -> (a -> Doc) -> Expr a -> Doc
    go1 !_ aDoc (Var a) = aDoc a
    go1 !_ _ (Int a) = Print.text $ show a
    go1 !depth aDoc (Add a b) = Print.hsep [go3 depth aDoc a, Print.char '+',  go2 depth aDoc b]
    go1 !depth aDoc (Call f xs) =
      Print.hsep $ go2 depth aDoc f : NonEmpty.toList (go2 depth aDoc <$> xs)
    go1 !depth aDoc (Lam n s) =
      Print.hsep
      [ Print.char lambda <> Print.braces (Print.hsep $ (Print.int . (+depth)) <$> [0..n-1]) <> Print.dot
      , go1 (depth+1) (unvar (Print.int . (+depth)) aDoc) (fromScope s)
      ]

    go2 :: Int -> (a -> Doc) -> Expr a -> Doc
    go2 !depth aDoc e@Var{} = go1 depth aDoc e
    go2 !depth aDoc e@Int{} = go1 depth aDoc e
    go2 !depth aDoc e@Add{} = go1 depth aDoc e
    go2 !depth aDoc e@Call{} = Print.parens $ go1 depth aDoc e
    go2 !depth aDoc e@Lam{} = Print.parens $ go1 depth aDoc e

    go3 :: Int -> (a -> Doc) -> Expr a -> Doc
    go3 !depth aDoc e@Var{} = go1 depth aDoc e
    go3 !depth aDoc e@Int{} = go1 depth aDoc e
    go3 !depth aDoc e@Add{} = Print.parens $ go1 depth aDoc e
    go3 !depth aDoc e@Call{} = go2 depth aDoc e
    go3 !depth aDoc e@Lam{} = go2 depth aDoc e

prettyLifted :: (a -> Doc) -> Lifted a -> Doc
prettyLifted aDoc (Lifted a b) =
  Print.hsep [aDoc a, Print.char '='] <>
  Print.nest 2 (prettyExpr absurd b)

instance Pretty a => Pretty (Expr a) where
  pretty = prettyExpr pretty

instance Pretty a => Pretty (Lifted a) where
  pretty = prettyLifted pretty
