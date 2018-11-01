{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Main where

import Hedgehog hiding (Var(..), check)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Bound (substitute)
import Bound.Scope (hoistScope, transverseScope, instantiate1)
import Control.Applicative ((<|>))
import Control.Monad (replicateM_)
import Control.Monad.State (MonadState, StateT, evalStateT, runStateT, modify, get, gets)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (runWriter)
import Data.Bifunctor (first)
import Data.Foldable (traverse_, toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import Data.Traversable (for)
import Data.Void (absurd)

import qualified Data.List.NonEmpty as NonEmpty

import Lambda
import Type
import Typecheck

genString :: MonadGen m => m String
genString = Gen.string (Range.constant 0 5) Gen.ascii

genExpr :: (MonadGen m, Eq a) => m a -> m (Expr a)
genExpr ma =
  Gen.recursive Gen.choice
    [ Var <$> ma ]
    [ Gen.subtermM2
        (genExpr ma)
        (genExpr ma)
        (\a b ->
           Gen.recursive Gen.choice
             [ pure $ Call a (pure b) ]
             [ Call a . (b :|) <$> Gen.list (Range.constant 1 9) (genExpr ma) ])
    , do
        s <- genExpr ma
        (\as -> lam as s) <$> Gen.filter (not . null) (Gen.subsequence $ toList s)
    ]

genType :: MonadGen m => m Type
genType =
  Gen.recursive Gen.choice
    [ pure Unit ]
    [ Gen.subterm2 genType genType Arr ]

genExprOfType :: (Eq a, MonadGen m) => m a -> Type -> StateT [(a, Type)] m (Expr a)
genExprOfType ma ty = do
  candidates <- gets $ filter ((==ty) . snd)
  Gen.recursive Gen.choice
    ([ Gen.element $ Var . fst <$> candidates | not (null candidates) ] <>
     [ -- new var
       do
         ctx <- get
         v <- Gen.filter (\a -> null $ filter ((==a) . fst) ctx) (lift ma)
         modify ((v, ty) :)
         pure $ Var v
     ])
    ([ do -- call
        (args, argTys) <- fmap NonEmpty.unzip . Gen.nonEmpty (Range.constant 1 10) $ genTypedExpr ma
        f <- genExprOfType ma $ foldr Arr ty argTys
        pure $ Call f args
    ] <>
    -- lambda
    case ty of
      Arr a b ->
        [ do
            expr <- genExprOfType ma b
            let frees = toList expr
            candidates' <- gets $ filter (\(x, y) -> x `elem` frees && y == a)
            Gen.choice
              ([ do -- abstract over a free var of the correct type
                   name <- fst <$> Gen.element candidates'
                   pure $ lam [name] expr
               | not (null candidates')
               ] <>
               [ -- abstract over nothing
                 pure $ Lam 1 (lift expr)
               ])
        ]
      _ -> [])

prop_genExprOfType_correct :: Property
prop_genExprOfType_correct =
  property $ do
    ty <- forAll genType
    (expr, ctx) <- forAll $ runStateT (genExprOfType genString ty) []
    assert $ check (flip lookup ctx) [] ty expr 

genTypedExpr :: forall a m. (Eq a, MonadGen m) => m a -> StateT [(a, Type)] m (Expr a, Type)
genTypedExpr ma = go
  where
    genVar :: StateT [(a, Type)] m (a, Type)
    genVar = do
      v <- lift ma
      ty <- do
        ty' <- gets $ lookup v
        case ty' of
          Nothing -> do
            ty'' <- lift genType
            modify ((v, ty'') :)
            pure ty''
          Just ty'' -> pure ty''
      pure (v, ty)

    genLam :: StateT [(a, Type)] m (Expr a, Type)
    genLam = do
      (expr, ty) <- go
      argNames <- Gen.filter (not . null) (Gen.subsequence $ toList expr)
      argTys <-
        for argNames $ \name -> do
          argTy <- gets $ fromJust . lookup name
          modify $ filter ((name==) . fst)
          pure argTy
      pure (lam argNames expr, foldr Arr ty argTys)

    go :: StateT [(a, Type)] m (Expr a, Type)
    go =
      Gen.recursive Gen.choice
        [ first Var <$> genVar ]
        [ genLam
        , do
            (f@(Lam n _), ty) <- genLam
            case unrollN n ty of
              Nothing -> undefined
              Just (argTys, retTy) -> do
                args <- traverse (genExprOfType ma) argTys
                pure (Call f (NonEmpty.fromList args), retTy)
        ]

prop_liftLambdas_correct :: Property
prop_liftLambdas_correct =
  property $ do
    expr <- forAll $ genExpr genString
    let
      supply = ("name"++) . show <$> [1::Int ..]
      (toplevel, ctx) = runWriter $ evalStateT (liftLambdas id expr) supply
    undefined

prop_check_test_1 :: Property
prop_check_test_1 =
  withTests 1 . withShrinks 0 . property $ do
    assert $ check (const Nothing) [] (Arr Unit Unit) (lam ["x"] $ Var "x")
    infer
      (flip lookup [("y", Unit)])
      []
      (Call (lam ["x"] $ Var "x") (pure $ Var "y")) ===
      Just Unit
    assert $
      check
        (flip lookup [("y", Unit)])
        []
        Unit
        (Call (lam ["x"] $ Var "x") (pure $ Var "y"))
    assert $
      check
        (flip lookup
         [ ( "x" , Arr Unit (Arr (Arr Unit Unit) Unit) )
         , ( "y" , Unit )
         ])
        []
        Unit
        (Call
           (Var "x")
           (Var "y" :| [ lam ["z"] $ Var "z" ]))

main :: IO Bool
main = checkParallel $$discover
