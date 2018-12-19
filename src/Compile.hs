{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
module Compile where

import Debug.Trace

import Bound.Scope (fromScope, toScope)
import Bound.Var (unvar)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Cont (ContT(..))
import Control.Monad.State (MonadState, gets, put, modify, execState)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Bifunctor (bimap, second)
import Data.Foldable (toList, traverse_, foldl')
import Data.IntMap (IntMap)
import Data.String (IsString(..))
import Data.Void (absurd)

import Defun
import Lambda (freshName)
import qualified Lambda (Expr)
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Grin.Grin as Grin (packName)
import qualified Grin.Syntax as Grin (Exp(..), Name, Val(..), Lit(..))
import qualified Grin.SyntaxDefs as Grin (Tag(..), TagType(..))
import qualified Pipeline.Pipeline as Grin (optimizeWith)
import qualified Pipeline.Definitions as Grin (Transformation(..), PipelineStep(..), Path(..), defaultOpts)
import qualified Pipeline.Utils as Grin (defaultOptimizations, defaultOnChange, defaultCleanUp)
import qualified Transformations.GenerateEval as Grin (generateEval)

arities :: Eq a => [Def a] -> a -> Maybe Int
arities [] _ = Nothing
arities (Def a n _:xs) a' =
  if a == a'
  then Just n
  else arities xs a'

defsToGrin
  :: (MonadState [a] m, Eq a)
  => (a -> Grin.Name)
  -> [Def a]
  -> m [(Grin.Name, [Grin.Name], Grin.Exp)]
defsToGrin name = traverse (defToGrin name)

defToGrin
  :: (Eq a, MonadState [a] m)
  => (a -> Grin.Name)
  -> Def a
  -> m (Grin.Name, [Grin.Name], Grin.Exp)
defToGrin name (Def n arity s) =
  uncurry ((,,) $ name n) <$> do
    names <- do; (xs, xs') <- gets (splitAt arity); fmap name xs <$ put xs'
    s' <-
      runContT
        (exprToGrin
           TopLevel
           arsA
           arsB
           name
           (unvar (names !!) name)
           (fromScope $ absurd <$> s))
        (pure . Grin.SReturn . Grin.Var)
    pure (names, s')
  where
    arsA x = arity <$ guard (x == n)
    arsB = unvar (const Nothing) arsA


collapse
  :: [Either (Grin.Exp -> Grin.Exp, Grin.Val) Grin.Val]
  -> (Grin.Exp -> Grin.Exp, [Grin.Val])
collapse [] = (id, [])
collapse (Left (g, x) : rest) = bimap (. g) (x :) (collapse rest)
collapse (Right x : rest) = second (x :) (collapse rest)

yieldExp
  :: MonadState [a] m
  => (a -> Grin.Name)
  -> Grin.Exp
  -> ContT Grin.Exp m Grin.Name
yieldExp nameA ea =
  ContT $ \f -> do
    n <- nameA <$> freshName
    Grin.EBind ea (Grin.Var n) <$> f n

cInt :: Grin.Val -> Grin.Val
cInt =
  Grin.ConstTagNode (Grin.Tag Grin.C $ Grin.packName "Int") .
  pure

evalAs
  :: MonadState [a] m
  => (a -> Grin.Name)
  -> (Grin.Name -> Grin.Val)
  -> Grin.Val
  -> ContT Grin.Exp m Grin.Name
evalAs nameA as xs =
  ContT $ \f -> do
    n <- nameA <$> freshName
    Grin.EBind (Grin.SApp (Grin.packName "eval") [xs]) (as n) <$> f n

data Level = TopLevel | Inner

exprToGrin
  :: MonadState [a] m
  => Level
  -> (a -> Maybe Int)
  -> (b -> Maybe Int)
  -> (a -> Grin.Name)
  -> (b -> Grin.Name)
  -> Defun a b
  -> ContT Grin.Exp m Grin.Name
exprToGrin _ _ _ nameA _ (Int n) =
  yieldExp nameA $
  Grin.SStore $
  cInt (Grin.Lit $ Grin.LInt64 n)
exprToGrin _ arsA arsB nameA nameB (Add a b) = do
  a' <-
    evalAs nameA (cInt . Grin.Var) .
    Grin.Var =<< exprToGrin Inner arsA arsB nameA nameB a
  b' <-
    evalAs nameA (cInt . Grin.Var) .
    Grin.Var =<< exprToGrin Inner arsA arsB nameA nameB b
  c <-
    yieldExp
    nameA
    (Grin.SApp (Grin.packName "_prim_int_add") [Grin.Var a', Grin.Var b'])
  yieldExp nameA $ Grin.SStore $ cInt $ Grin.Var c
exprToGrin level _ _ nameA nameB (Var a) =
  case level of
    Inner -> pure $ nameB a
    TopLevel -> evalAs nameA Grin.Var (Grin.Var $ nameB a)
exprToGrin level arsA arsB nameA nameB (App f xs) = do
  f' <- exprToGrin Inner arsA arsB nameA nameB f
  xs' <- traverse (exprToGrin Inner arsA arsB nameA nameB) (toList xs)
  case level of
    Inner ->
      yieldExp nameA . Grin.SStore $
        Grin.ConstTagNode
          (Grin.Tag Grin.F . Grin.packName $ "ap" <> show (length xs))
          (Grin.Var f' : fmap Grin.Var (toList xs'))
    TopLevel ->
      yieldExp nameA $
        Grin.SApp
          (Grin.packName $ "ap" <> show (length xs))
          (Grin.Var f' : fmap Grin.Var (toList xs'))
exprToGrin _ arsA _ nameA _ (Global a) =
  case arsA a of
    Nothing -> error "no arity for global function"
    Just ar ->
      yieldExp nameA . Grin.SStore $
        Grin.ConstTagNode
          (Grin.Tag (Grin.P ar) $ nameA a)
          []
exprToGrin level arsA arsB nameA nameB (Call a xs) = do
  xs' <- traverse (exprToGrin Inner arsA arsB nameA nameB) (toList xs)
  case arsA a of
    Nothing -> yieldExp nameA $ Grin.SApp (nameA a) (Grin.Var <$> xs')
    Just ar -> do
      let missing = ar - length xs
      if missing == 0
        then
          yieldExp nameA . Grin.SStore $
            Grin.ConstTagNode
              (Grin.Tag Grin.F $ nameA a)
              (Grin.Var <$> xs')
        else
          yieldExp nameA . Grin.SStore $
            Grin.ConstTagNode
              (Grin.Tag (Grin.P missing) $ nameA a)
              (Grin.Var <$> xs')

genAps :: [Def a] -> [(Grin.Name, [Grin.Name], Grin.Exp)]
genAps ds = toList $ execState (go ds) IntMap.empty
  where
    go :: MonadState (IntMap (Grin.Name, [Grin.Name], Grin.Exp)) m => [Def a] -> m ()
    go [] = pure ()
    go (Def _ _ e:xs) = goExpr (fromScope e) *> go xs

    genAp :: Int -> (Grin.Name, [Grin.Name], Grin.Exp)
    genAp n =
      ( Grin.packName $ "ap" <> show n
      , args
      , foldl'
          (\b (name, a) ->
             Grin.EBind b name $
             Grin.SApp (Grin.packName "apply") [name, Grin.Var a])
          (Grin.SApp (Grin.packName "eval") [Grin.Var $ head args])
          (zip (tail binds) (tail args))
      )
      where
        args = Grin.packName . ("x" <>) . show <$> [0..n]
        binds = Grin.Var . Grin.packName . ("b" <>) . show <$> [0..n]

    goExpr :: MonadState (IntMap (Grin.Name, [Grin.Name], Grin.Exp)) m => Defun a b -> m ()
    goExpr e =
      case e of
        App f xs -> do
          goExpr f
          traverse_ goExpr xs
          let lxs = length xs
          res <- gets $ IntMap.member lxs
          if res
            then pure ()
            else modify $ IntMap.insert lxs (genAp lxs)
        Call _ xs -> traverse_ goExpr xs
        Add a b -> goExpr a *> goExpr b
        Global _ -> pure ()
        Var _ -> pure ()
        Int _ -> pure ()

exprToProgram
  :: (Show a, IsString a, Semigroup a, Eq a, MonadState [a] m)
  => (a -> Grin.Name)
  -> Lambda.Expr a
  -> m Grin.Exp
exprToProgram name e = do
  (e1, defs1) <- defun e
  defs2 <- (genAps defs1 <>) <$> traverse (defToGrin name) defs1
  let ars2 a = lookup (name a) $ (\(b, c, _) -> (b, length c)) <$> defs2
  e3 <- flip runContT (pure . Grin.SReturn . Grin.Var) $ do
    e3 <- exprToGrin TopLevel ars2 ars2 name name e1
    res <- evalAs name (cInt . Grin.Var) (Grin.Var e3)
    yieldExp name $ Grin.SApp (Grin.packName "_prim_int_print") [Grin.Var res]
  let
    p = Grin.Program $
      fmap (\(a, b, c) -> Grin.Def a b c) defs2 <>
      [ Grin.Def (Grin.packName "grinMain") [] e3
      ]
  pure $ Grin.generateEval p

optimizeProgram :: Grin.Exp -> IO Grin.Exp
optimizeProgram e =
  Grin.optimizeWith
    Grin.defaultOpts
    e
    []
    Grin.defaultOptimizations
    (Grin.T Grin.InlineEval : Grin.defaultOnChange)
    Grin.defaultCleanUp
    [Grin.PrintAST, Grin.SaveLLVM True "out", Grin.SaveGrin $ Grin.Rel "grin"]
