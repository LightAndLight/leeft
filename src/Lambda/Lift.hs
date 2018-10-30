{-# language FlexibleContexts #-}
module Lambda.Lift where

import Control.Lens.Fold ((^..))
import Control.Lens.Plated (cosmos, transformM)
import Control.Monad.State (MonadState, get, put)
import Control.Monad.Writer (MonadWriter, runWriterT, tell)

import Lambda.Expr
import Lambda.Subst

liftLambdas
  :: MonadState [String] m
  => Expr
  -> m (Expr, [(String, Expr)])
liftLambdas = runWriterT . transformM go
  where
    go
      :: (MonadState [String] m, MonadWriter [(String, Expr)] m)
      => Expr
      -> m Expr
    go e@Lam{} = do
      n:ns <- get; put ns
      let
        frees = e ^.. cosmos._Free
        e' = foldr abstract e frees
      tell [(n, e')]
      pure $ foldl (\f -> App f . Free) (Free n) frees
    go e = pure e
