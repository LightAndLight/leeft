{-# language FlexibleContexts #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language StandaloneDeriving, TemplateHaskell #-}
{-# language ScopedTypeVariables #-}
module Scratchpad where

import Bound
import Bound.Scope
import Bound.Var
import Control.Monad (replicateM)
import Control.Monad.State
  (MonadState, get, gets, put, runState, evalState, evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (MonadWriter, tell, runWriter, runWriterT)
import Data.Bifunctor (bimap)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Foldable (toList)
import Data.List (elemIndex, nub, union)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import Data.Void (Void, absurd)

import qualified OrderedSet as Ordered

data Expr a
  = Var a
  | App (Expr a) (Expr a)
  | Lam Int (Scope Int Expr a)
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
      let
        l = length as
      in
        Lam (l + bs) $ toScope $ unvar (B . (l+)) (\a -> maybe (F a) B $ elemIndex a as) <$> fromScope s
    _ -> Lam (length as) $ abstract (`elemIndex` as) e

-- | If a term is closed then it can be cast to other types
cast :: Expr a -> Maybe (Expr b)
cast = traverse (const Nothing)

replace :: a -> Expr a -> (Bool, Expr a)
replace val e =
  case e of
    Var a -> (False, Var a)
    App f x ->
      case replace val f of
        (True, f') -> (True, App f' x)
        (False, _) -> App f <$> replace val x
    Lam as s ->
      case replace' val s of
        (True, s') -> (True, Lam as s')
        (False, _) -> (True, Var val)
  where
    replace' :: b -> Scope Int Expr b -> (Bool, Scope Int Expr b)
    replace' b = fmap toScope . replace (F b) . fromScope

replace2 :: Eq a => a -> Expr a -> (Maybe (Expr a), Expr a)
replace2 val e =
  case e of
    Var a -> (Nothing, Var a)
    App f x ->
      case replace2 val f of
        (Just e, f') -> (Just e, App f' x)
        (Nothing, _) -> App f <$> replace2 val x
    Lam as s ->
      case replace2' val s of
        (Just e, s') -> (Just e, Lam as s')
        (Nothing, _) ->
          let
            frees = nub $ toList e
          in
            (cast $ lam frees e, Var val)
  where
    replace2' :: Eq b => b -> Scope Int Expr b -> (Maybe (Expr b), Scope Int Expr b)
    replace2' b = bimap (>>= cast) toScope . replace2 (F b) . fromScope

cdr :: (a, b, c) -> (b, c)
cdr (_, b, c) = (b, c)

replace3 :: Eq a => [a] -> Expr a -> ([a], [(a, Expr a)], Expr a)
replace3 supply e =
  case e of
    Var a -> (supply, [], Var a)
    App f x ->
      let
        (supply', es, f') = replace3 supply f
        (supply'', es', x') = replace3 supply' x
      in
        (supply'', es ++ es', App f' x')
    Lam as s ->
      case replace3' supply s of
        (supply', es, s') ->
          case supply' of
            n:supply'' ->
              let
                frees = nub $ toList s'
              in
                (supply'', (n, lam frees e):es, foldr (\x f -> App f (Var x)) (Var n) frees)
  where
    replace3' :: Eq b => [b] -> Scope Int Expr b -> ([b], [(b, Expr b)], Scope Int Expr b)
    replace3' supply =
      (\(x, y, z) -> (unvar undefined id <$> x, bimap (unvar undefined id) (fromJust . cast) <$> y, toScope z)) .
      replace3 (F <$> supply) .
      fromScope

replace4 :: forall a b. (Eq a, Eq b) => [a] -> (a -> b) -> Expr b -> ([a], [(a, Expr b)], Expr b)
replace4 supply ctx e =
  case e of
    Var a -> (supply, [], Var a)
    App f x ->
      let
        (supply', es, f') = replace4 supply ctx f
        (supply'', es', x') = replace4 supply' ctx x
      in
        (supply'', es ++ es', App f' x')
    Lam as s ->
      case replace4' supply ctx s of
        (supply', es, s') ->
          case supply' of
            n:supply'' ->
              let
                frees = nub $ toList s'
              in
                (supply'', (n, lam frees e):es, foldr (\x f -> App f (Var x)) (Var $ ctx n) frees)
  where
    replace4' :: Eq b => [a] -> (a -> b) -> Scope Int Expr b -> ([a], [(a, Expr b)], Scope Int Expr b)
    replace4' supply ctx =
      (\(x, y, z) -> (x, bimap id (fromJust . cast) <$> y, toScope z)) .
      replace4 supply (F . ctx) .
      fromScope

toClosed :: Expr a -> Maybe (Expr Void)
toClosed = traverse (const Nothing)

replace5 :: forall a b. (Eq a, Eq b) => [a] -> (a -> b) -> Expr b -> ([a], [(a, Expr Void)], Expr b)
replace5 supply ctx e =
  case e of
    Var a -> (supply, [], Var a)
    App f x ->
      let
        (supply', es, f') = replace5 supply ctx f
        (supply'', es', x') = replace5 supply' ctx x
      in
        (supply'', es ++ es', App f' x')
    Lam as s ->
      case replace5' supply ctx s of
        (supply', es, s') ->
          case supply' of
            n:supply'' ->
              let
                frees = nub $ toList s'
              in
                (supply'', (n, fromJust $ toClosed $ lam frees e):es, foldr (\x f -> App f (Var x)) (Var $ ctx n) frees)
  where
    replace5' :: Eq b => [a] -> (a -> b) -> Scope Int Expr b -> ([a], [(a, Expr Void)], Scope Int Expr b)
    replace5' supply ctx =
      (\(x, y, z) -> (x, y, toScope z)) .
      replace5 supply (F . ctx) .
      fromScope

replace6
  :: forall a b m
   . ( MonadState [a] m, MonadWriter [(a, Expr Void)] m
     , Eq a, Eq b
     )
  => (a -> b) -> Expr b -> m (Expr b)
replace6 ctx e =
  case e of
    Var a -> pure $ Var a
    App f x -> do
      f' <- replace6 ctx f
      x' <- replace6 ctx x
      pure $ App f' x'
    Lam as s -> do
      s' <- replace6' ctx s
      n <- do; x:xs <- get; put xs; pure x
      let frees = nub $ toList s'
      tell [(n, fromJust $ toClosed $ lam frees $ Lam as s')]
      pure $ foldr (\x f -> App f (Var x)) (Var $ ctx n) frees
  where
    replace6'
      :: ( MonadState [a] m, MonadWriter [(a, Expr Void)] m
         , Eq b
         )
      => (a -> b) -> Scope Int Expr b -> m (Scope Int Expr b)
    replace6' ctx = fmap toScope . replace6 (F . ctx) . fromScope

data Expr' a
  = Var' a
  | Call' (Expr' a) (NonEmpty (Expr' a))
  | Lam' Int (Scope Int Expr' a)
  deriving (Functor, Foldable, Traversable)
deriveShow1 ''Expr'
deriveEq1 ''Expr'
makeBound ''Expr'
deriving instance Show a => Show (Expr' a)
deriving instance Eq a => Eq (Expr' a)

lam' :: Eq a => [a] -> Expr' a -> Expr' a
lam' as e =
  case e of
    Lam' bs s ->
      let
        l = length as
      in
        Lam' (l + bs) $ toScope $ fmap (unvar (B . (l+)) (\a -> maybe (F a) B $ elemIndex a as)) $ fromScope s
    _ -> Lam' (length as) $ abstract (`elemIndex` as) e

toClosed' :: Expr' a -> Maybe (Expr' Void)
toClosed' = traverse (const Nothing)

replace7
  :: forall a m
   . ( MonadState [a] m, MonadWriter [(a, Expr' Void)] m)
  => forall b. Eq b => (a -> b) -> Expr' b -> m (Expr' b)
replace7 ctx e =
  case e of
    Var' a -> pure $ Var' a
    Call' f xs -> do
      f' <- replace7 ctx f
      xs' <- traverse (replace7 ctx) xs
      pure $ Call' f' xs'
    Lam' as s -> do
      s' <- replace7' ctx s
      n <- do; x:xs <- get; put xs; pure x
      let frees = nub $ toList s'
      tell [(n, fromJust $ toClosed' $ lam' frees $ Lam' as s')]
      pure $ case Var' <$> frees of
        [] -> Var' $ ctx n
        v:vs -> Call' (Var' $ ctx n) (v:|vs)
  where
    replace7' :: Eq b => (a -> b) -> Scope Int Expr' b -> m (Scope Int Expr' b)
    replace7' ctx = fmap toScope . replace7 (F . ctx) . fromScope

replace8
  :: forall a m
   . ( MonadState [a] m, MonadWriter [(a, Expr' Void)] m)
  => forall b. Eq b => (a -> b) -> Expr' b -> m (Expr' b)
replace8 ctx e =
  case e of
    Var' a -> pure $ Var' a
    Call' f xs -> do
      f' <- replace8 ctx f
      xs' <- traverse (replace8 ctx) xs
      pure $ Call' f' xs'
    Lam' as s -> do
      s' <- replace8' ctx s
      n <- do; x:xs <- get; put xs; pure x
      case ripped $ Lam' as s' of
        (tm, xs) -> do
          tell [(n, tm)]
          pure $ case Var' <$> xs of
            [] -> Var' $ ctx n
            v:vs -> Call' (Var' $ ctx n) $ v :| vs
  where
    replace8' :: forall b. Eq b => (a -> b) -> Scope Int Expr' b -> m (Scope Int Expr' b)
    replace8' ctx = fmap toScope . replace8 (F . ctx) . fromScope

    ripped :: forall b. Eq b => Expr' b -> (Expr' Void, [b])
    ripped e =
      ( Lam' (length frees) $ abstractEither (Left . fromJust . (`elemIndex` frees)) e
      , frees
      )
      where
        frees = nub $ toList e

replace9
  :: forall a m
   . ( MonadState [a] m, MonadWriter [(a, Expr' Void)] m)
  => forall b. Eq b => (a -> b) -> Expr' b -> m (Expr' b)
replace9 ctx e =
  case e of
    Var' a -> pure $ Var' a
    Call' f xs -> do
      f' <- replace9 ctx f
      xs' <- traverse (replace9 ctx) xs
      pure $ Call' f' xs'
    Lam' as s -> do
      s' <- replace9' ctx s
      n <- do; x:xs <- get; put xs; pure x
      case ripped s' of
        (s'', xs) -> do
          tell [(n, Lam' (as + length xs) s'')]
          pure $ case Var' <$> xs of
            [] -> Var' $ ctx n
            v:vs -> Call' (Var' $ ctx n) $ v :| vs
  where
    replace9' :: forall b. Eq b => (a -> b) -> Scope Int Expr' b -> m (Scope Int Expr' b)
    replace9' ctx = fmap toScope . replace9 (F . ctx) . fromScope

    ripped :: forall b. Eq b => Scope Int Expr' b -> (Scope Int Expr' Void, [b])
    ripped s =
      ( toScope $ unvar (B . (+l)) (B . fromJust . (`elemIndex` frees)) <$> fromScope s
      , frees
      )
      where
        l = length frees
        frees = nub $ toList s

replace10
  :: forall a m
   . ( MonadState [a] m, MonadWriter [(a, Expr' Void)] m)
  => forall b. Eq b => (a -> b) -> Expr' b -> m (Expr' b)
replace10 ctx e =
  case e of
    Var' a -> pure $ Var' a
    Call' f xs -> do
      f' <- replace10 ctx f
      xs' <- traverse (replace10 ctx) xs
      pure $ Call' f' xs'
    Lam' as s -> do
      s' <- replace10' ctx s
      n <- do; x:xs <- get; put xs; pure x
      case ripped s' of
        (s'', xs) -> do
          tell [(n, Lam' (as + length xs) s'')]
          pure $ case Var' <$> xs of
            [] -> Var' $ ctx n
            v:vs -> Call' (Var' $ ctx n) $ v :| vs
  where
    replace10' :: forall b. Eq b => (a -> b) -> Scope Int Expr' b -> m (Scope Int Expr' b)
    replace10' ctx = fmap toScope . replace10 (F . ctx) . fromScope

    ripped :: forall b. Eq b => Scope Int Expr' b -> (Scope Int Expr' Void, [b])
    ripped s = (res, frees)
      where
        res =
          evalState
            (fmap toScope $
             traverse
               (unvar
                  (pure . B . (+l))
                  (\a -> do
                      (n, st) <- gets $ Ordered.posInsertMay a
                      case st of
                        Nothing -> pure $ B n
                        Just st' -> B n <$ put st')) $
             fromScope s)
            Ordered.empty

        l = length frees
        frees = nub $ toList s

replace11
  :: forall a m
   . ( MonadState [a] m, MonadWriter [(a, Expr' Void)] m)
  => forall b. Eq b => (a -> b) -> Expr' b -> m (Expr' b)
replace11 ctx e =
  case e of
    Var' a -> pure $ Var' a
    Call' f xs -> do
      f' <- replace11 ctx f
      xs' <- traverse (replace11 ctx) xs
      pure $ Call' f' xs'
    Lam' as s -> do
      s' <- replace11' ctx s
      n <- do; x:xs <- get; put xs; pure x
      case ripped s' of
        (s'', xs) -> do
          tell [(n, Lam' (as + length xs) s'')]
          pure $ case Var' <$> xs of
            [] -> Var' $ ctx n
            v:vs -> Call' (Var' $ ctx n) $ v :| vs
  where
    replace11' :: forall b. Eq b => (a -> b) -> Scope Int Expr' b -> m (Scope Int Expr' b)
    replace11' ctx = fmap toScope . replace11 (F . ctx) . fromScope

    ripped :: forall b. Eq b => Scope Int Expr' b -> (Scope Int Expr' Void, [b])
    ripped s = (res, Ordered.toList st)
      where
        l = Ordered.size st
        (res, st) =
          runState
            (fmap toScope $
              traverse
                (unvar
                  (pure . B . (+l))
                  (\a -> do
                    (n, st) <- gets $ Ordered.posInsertMay a
                    case st of
                      Nothing -> pure $ B n
                      Just st' -> B n <$ put st')) $
              fromScope s)
            Ordered.empty