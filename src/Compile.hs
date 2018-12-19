{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
module Compile where

import Bound.Scope (fromScope)
import Bound.Var (Var(..), unvar)
import Control.Monad (guard, replicateM)
import Control.Monad.Cont (ContT(..))
import Control.Monad.State (MonadState, gets, modify, execState)
import Data.Foldable (toList, traverse_, foldl')
import Data.IntMap (IntMap)
import Data.String (IsString(..))
import Data.Void (absurd)

import qualified Data.IntMap as IntMap
import qualified Grin.Grin as Grin (packName)
import qualified Grin.Syntax as Grin (Exp(..), Name, Val(..), Lit(..))
import qualified Grin.SyntaxDefs as Grin (Tag(..), TagType(..))
import qualified Pipeline.Pipeline as Grin (optimizeWith)
import qualified Pipeline.Definitions as Grin (Transformation(..), PipelineStep(..), Path(..), defaultOpts)
import qualified Pipeline.Utils as Grin (defaultOnChange, defaultCleanUp)
import qualified Transformations.GenerateEval as Grin (generateEval)

import Defun
import Fresh.Class (MonadFresh, fresh)

import qualified Lambda (Expr)

newtype ExpBuilderT m a = ExpBuilderT { unExpBuilderT :: ContT Grin.Exp m a }
  deriving (Functor, Applicative, Monad)

runExpBuilderT :: ExpBuilderT m a -> (a -> m Grin.Exp) -> m Grin.Exp
runExpBuilderT (ExpBuilderT m) = runContT m

yieldExp
  :: MonadFresh s a m
  => (a -> Grin.Name)
  -> Grin.Exp
  -> ExpBuilderT m Grin.Name
yieldExp nameA ea =
  ExpBuilderT .
  ContT $ \f -> do
    n <- nameA <$> fresh
    Grin.EBind ea (Grin.Var n) <$> f n

evalAs
  :: MonadFresh s a m
  => (a -> Grin.Name)
  -> (Grin.Name -> Grin.Val)
  -> Grin.Val
  -> ExpBuilderT m Grin.Name
evalAs nameA as xs =
  ExpBuilderT .
  ContT $ \f -> do
    n <- nameA <$> fresh
    Grin.EBind (Grin.SApp (Grin.packName "eval") [xs]) (as n) <$> f n

defsToGrin
  :: (MonadFresh s a m, Eq a)
  => (a -> Grin.Name)
  -> [Def a]
  -> m [(Grin.Name, [Grin.Name], Grin.Exp)]
defsToGrin name = traverse (defToGrin name)

defToGrin
  :: (Eq a, MonadFresh s a m)
  => (a -> Grin.Name)
  -> Def a
  -> m (Grin.Name, [Grin.Name], Grin.Exp)
defToGrin name (Def n arity s) =
  uncurry ((,,) $ name n) <$> do
    names <- replicateM arity $ name <$> fresh
    s' <-
      runExpBuilderT
        (exprToGrin
           TopLevel
           F
           arsB
           (unvar (names !!) name)
           (fromScope $ absurd <$> s))
        (pure . Grin.SReturn . Grin.Var)
    pure (names, s')
  where
    arsA x = arity <$ guard (x == n)
    arsB = unvar (const Nothing) arsA

cInt :: Grin.Name -> Grin.Val
cInt =
  Grin.ConstTagNode (Grin.Tag Grin.C $ Grin.packName "Int") .
  pure .
  Grin.Var

mkCInt :: Grin.Val -> Grin.Val
mkCInt =
  Grin.ConstTagNode (Grin.Tag Grin.C $ Grin.packName "Int") .
  pure

data Level = TopLevel | Inner

exprToGrin
  :: MonadFresh s a m
  => Level
  -> (a -> b)
  -> (b -> Maybe Int)
  -> (b -> Grin.Name)
  -> Defun a b
  -> ExpBuilderT m Grin.Name
exprToGrin _ ctx _ nameB (Int n) =
  yieldExp (nameB . ctx) $
  Grin.SStore $
  mkCInt (Grin.Lit $ Grin.LInt64 n)
exprToGrin _ ctx arsB nameB (Add a b) = do
  a' <-
    evalAs (nameB . ctx) cInt .
    Grin.Var =<< exprToGrin Inner ctx arsB nameB a
  b' <-
    evalAs (nameB . ctx) cInt .
    Grin.Var =<< exprToGrin Inner ctx arsB nameB b
  c <-
    yieldExp
    (nameB . ctx)
    (Grin.SApp (Grin.packName "_prim_int_add") [Grin.Var a', Grin.Var b'])
  yieldExp (nameB . ctx) $ Grin.SStore $ cInt c
exprToGrin level ctx _ nameB (Var a) =
  case level of
    Inner -> pure $ nameB a
    TopLevel -> evalAs (nameB . ctx) Grin.Var (Grin.Var $ nameB a)
exprToGrin level ctx arsB nameB (App f xs) = do
  f' <- exprToGrin Inner ctx arsB nameB f
  xs' <- traverse (exprToGrin Inner ctx arsB nameB) (toList xs)
  case level of
    Inner ->
      yieldExp (nameB . ctx) . Grin.SStore $
        Grin.ConstTagNode
          (Grin.Tag Grin.F . Grin.packName $ "ap" <> show (length xs))
          (Grin.Var f' : fmap Grin.Var (toList xs'))
    TopLevel -> do
      a <-
        yieldExp (nameB . ctx) . Grin.SStore $
          Grin.ConstTagNode
            (Grin.Tag Grin.F . Grin.packName $ "ap" <> show (length xs))
            (Grin.Var f' : fmap Grin.Var (toList xs'))
      evalAs (nameB . ctx) Grin.Var (Grin.Var a)
exprToGrin _ ctx arsB nameB (Global a) =
  case arsB $ ctx a of
    Nothing -> error "no arity for global function"
    Just ar ->
      yieldExp (nameB . ctx) . Grin.SStore $
        Grin.ConstTagNode
          (Grin.Tag (Grin.P ar) $ nameB $ ctx a)
          []
exprToGrin _ ctx arsB nameB (Call a xs) = do
  xs' <- traverse (exprToGrin Inner ctx arsB nameB) (toList xs)
  case arsB $ ctx a of
    Nothing -> yieldExp (nameB . ctx) $ Grin.SApp (nameB $ ctx a) (Grin.Var <$> xs')
    Just ar -> do
      let missing = ar - length xs
      if missing == 0
        then
          yieldExp (nameB . ctx) . Grin.SStore $
            Grin.ConstTagNode
              (Grin.Tag Grin.F $ nameB $ ctx a)
              (Grin.Var <$> xs')
        else
          yieldExp (nameB . ctx) . Grin.SStore $
            Grin.ConstTagNode
              (Grin.Tag (Grin.P missing) $ nameB $ ctx a)
              (Grin.Var <$> xs')

genAps :: [Def a] -> [(Grin.Name, [Grin.Name], Grin.Exp)]
genAps ds = toList $ execState (go ds) IntMap.empty
  where
    go
      :: MonadState (IntMap (Grin.Name, [Grin.Name], Grin.Exp)) m
      => [Def a]
      -> m ()
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

    goExpr
      :: MonadState (IntMap (Grin.Name, [Grin.Name], Grin.Exp)) m
      => Defun a b
      -> m ()
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
  :: (Show a, IsString a, Semigroup a, Eq a, MonadFresh s a m)
  => (a -> Grin.Name)
  -> Lambda.Expr a
  -> m Grin.Exp
exprToProgram name e = do
  (e1, defs1) <- defun e
  defs2 <- (genAps defs1 <>) <$> traverse (defToGrin name) defs1
  let arities a = lookup (name a) $ (\(b, c, _) -> (b, length c)) <$> defs2
  e3 <- flip runExpBuilderT (pure . Grin.SReturn . Grin.Var) $ do
    e3 <- exprToGrin TopLevel id arities name e1
    res <- evalAs name cInt (Grin.Var e3)
    yieldExp name $ Grin.SApp (Grin.packName "_prim_int_print") [Grin.Var res]
  let
    p = Grin.Program $
      fmap (\(a, b, c) -> Grin.Def a b c) defs2 <>
      [ Grin.Def (Grin.packName "grinMain") [] e3
      ]
  pure $ Grin.generateEval p

--- optimization

op :: [Grin.Transformation]
op =
  [ Grin.InlineEval
  , Grin.InlineApply
  , Grin.EvaluatedCaseElimination
  , Grin.TrivialCaseElimination
  , Grin.SparseCaseOptimisation
  , Grin.UpdateElimination
  , Grin.NonSharedElimination
  , Grin.CopyPropagation
  , Grin.ConstantPropagation
  , Grin.SimpleDeadFunctionElimination
  , Grin.SimpleDeadParameterElimination
  , Grin.SimpleDeadVariableElimination
  , Grin.DeadCodeElimination
  , Grin.CommonSubExpressionElimination
  , Grin.CaseCopyPropagation
  , Grin.CaseHoisting
  , Grin.GeneralizedUnboxing
  , Grin.ArityRaising
  , Grin.LateInlining
  ]

optimizeProgram :: Grin.Exp -> IO Grin.Exp
optimizeProgram e =
  Grin.optimizeWith
    Grin.defaultOpts
    e
    []
    op
    Grin.defaultOnChange
    Grin.defaultCleanUp
    [Grin.PrintAST, Grin.SaveLLVM True "out", Grin.SaveGrin $ Grin.Rel "grin"]