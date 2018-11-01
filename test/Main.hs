{-# language TemplateHaskell #-}
module Main where

import Hedgehog hiding (Var(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Bound (substitute)
import Bound.Scope (hoistScope, transverseScope, instantiate1)
import Control.Applicative ((<|>))
import Control.Monad (replicateM_)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (runWriter)
import Data.Foldable (traverse_, toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import Data.Void (absurd)

import qualified Data.List.NonEmpty as NonEmpty

import Lambda

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
    expr <- forAll $ genExpr (Gen.string (Range.constant 0 5) Gen.ascii)
    let
      supply = ("name"++) . show <$> [1::Int ..]
      (toplevel, ctx) = runWriter $ evalStateT (liftLambdas id expr) supply
    undefined

main :: IO Bool
main = do
  replicateM_ 10 $ do
    e <- Gen.sample $ genExpr (Gen.string (Range.constant 0 5) Gen.ascii)
    let
      supply = ("name"++) . show <$> [1::Int ..]
    print e
    putStrLn ""
    print . runWriter $ evalStateT (liftLambdas id e) supply
    putStrLn "\n"
  checkParallel $$discover
