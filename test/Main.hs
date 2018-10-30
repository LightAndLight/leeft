module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalState)
import Test.Hspec

import Lambda.Expr
import Lambda.Lift
import Lambda.Subst

supply :: [String]
supply = ("name"++) . show <$> [1..]

main :: IO ()
main =
  hspec $ do
    describe "lambda lifting" $ do
      it "example 1" $ do
        let tm = abstract "x" $ App (Free "x") (Free "y")
        liftIO . print $ evalState (liftLambdas tm) supply
        () `shouldBe` ()
      it "example 2" $ do
        let tm = abstract "x" $ App (Free "x") (abstract "x" $ App (Free "x") (Free "y"))
        liftIO . print $ evalState (liftLambdas tm) supply
        () `shouldBe` ()
      it "example 3" $ do
        let tm = abstract "x" $ App (App (Free "x") (Free "y")) (abstract "x" $ App (Free "y") (Free "x"))
        liftIO . print $ evalState (liftLambdas tm) supply
        () `shouldBe` ()
