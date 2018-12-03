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

prop_liftLambdas_correct :: Property
prop_liftLambdas_correct =
  property $ do
    expr <- forAll $ genExpr genString
    let
      supply = ("name"++) . show <$> [1::Int ..]
      (toplevel, ctx) = runWriter $ evalStateT (liftLambdas id expr) supply
    undefined

main :: IO Bool
main = checkParallel $$discover
