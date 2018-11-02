{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances, UndecidableInstances #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
module Unification
  ( -- * Monad
    UnifyT
  , runUnifyT
  , UVar
    -- * Operations
  , freshVar
  , fresh
  , occurs
  , union
  , find
  , unify
  , unfreeze
  , freeze
    -- * Type classes
  , Unifiable(..)
  , AsVar(..)
  , AsUnificationError(..)
  , UnificationError(..)
  , HasAnnotation(..)
  )
where

import Bound.Scope (Scope, fromScope, instantiateVars)
import Bound.Util (applySome)
import Control.Lens.Fold ((^?), (^..), folded, lengthOf, nullOf)
import Control.Lens.Getter ((^.), view, use)
import Control.Lens.Iso (Iso, from)
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Prism (Prism', prism', _Left)
import Control.Lens.Review ((#))
import Control.Lens.Setter ((.=), (+=), (-=))
import Control.Lens.TH (makeLenses, makeWrapped)
import Control.Lens.Traversal (traverseOf)
import Control.Lens.Wrapped (_Unwrapped)
import Control.Monad (join, unless, replicateM)
import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState(..), StateT(..), evalStateT)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Deriving ()
import Data.Function (on)
import Data.Functor.Classes (Eq1, Show1, Ord1)
import Data.Functor.Compose (Compose)
import Data.Equivalence.Monad (EquivT, runEquivT, classDesc, equate)
import Data.Foldable hiding (find)

import Type

-- | Unification variables. Use 'freshVar' and 'fresh' to obtain new 'UVar's
data UVar
  = UVar
  { _uvId :: Int
  , _uvDepth :: Int
  } deriving (Eq, Show, Ord)

newtype UTerm t a = UTerm { unUTerm :: Compose t (Either UVar) a }
deriving instance (Show1 t, Show a) => Show (UTerm t a)
deriving instance (Eq1 t, Eq a) => Eq (UTerm t a)
deriving instance (Ord1 t, Ord a) => Ord (UTerm t a)

uterm :: Iso (t (Either UVar a)) (t' (Either UVar a')) (UTerm t a) (UTerm t' a') 
uterm = _Unwrapped._Unwrapped

-- | Concrete unification error datatype
data UnificationError term var ann
  = OccursError UVar (UTerm term var) ann
  | MismatchError (UTerm term var) (UTerm term var) ann
  | EscapedVariables [UVar] (UTerm term var) ann
  deriving (Eq, Show)

instance AsUnificationError (UnificationError term var ann) term var ann where
  _OccursError =
    prism'
      (\(a, b, c) -> OccursError a b c)
      (\case
          OccursError a b c -> Just (a, b, c)
          _ -> Nothing)

  _MismatchError =
    prism'
      (\(a, b, c) -> MismatchError a b c)
      (\case
          MismatchError a b c -> Just (a, b, c)
          _ -> Nothing)

  _EscapedVariables =
    prism'
      (\(a, b, c) -> EscapedVariables a b c)
      (\case
          EscapedVariables a b c -> Just (a, b, c)
          _ -> Nothing)

newtype E a b c d = E { getE :: forall s. EquivT s a b c d }

instance Functor c => Functor (E a b c) where
  fmap f (E a) = E $ fmap f a

instance (Applicative c, Monad c) => Applicative (E a b c) where
  pure a = E $ pure a
  E f <*> E a = E $ f <*> a

instance Monad c => Monad (E a b c) where
  E a >>= f = E $ a >>= (getE . f)

data UnifyState
  = UnifyState
  { _usNextId :: !Int
  , _usDepth :: !Int
  }

newtype UnifyT t v m a
  = UnifyT
  { runUnifyT'
    :: StateT UnifyState (E (UTerm t v) (UTerm t v) m) a
  }

instance Functor m => Functor (UnifyT t v m) where
  fmap f (UnifyT a) = UnifyT $ fmap f a

instance (Applicative m, Monad m) => Applicative (UnifyT t v m) where
  pure a = UnifyT $ pure a
  UnifyT f <*> UnifyT a = UnifyT $ f <*> a

instance Monad m => Monad (UnifyT t v m) where
  UnifyT a >>= f = UnifyT $ a >>= (runUnifyT' . f)

instance MonadState s m => MonadState s (UnifyT t v m) where
  get = UnifyT . lift $ E get
  put a = UnifyT . lift $ E (put a)

instance MonadError e m => MonadError e (UnifyT t v m) where
  catchError a b =
    UnifyT . StateT $ \s -> E ((,) <$> catchError (getE . flip evalStateT s $ runUnifyT' a) (getE . flip evalStateT s . runUnifyT' . b) <*> pure s)
  throwError a = UnifyT . lift $ E (throwError a)

instance MonadWriter w m => MonadWriter w (UnifyT t v m) where
  writer a = UnifyT . lift $ E (writer a)
  tell a = UnifyT . lift $ E (tell a)
  listen a =
    UnifyT $ StateT $ \s -> E ((,) <$> listen (getE . flip evalStateT s $ runUnifyT' a) <*> pure s)
  pass a =
    UnifyT $ StateT $ \s -> E ((,) <$> pass (getE . flip evalStateT s $ runUnifyT' a) <*> pure s)

instance MonadReader r m => MonadReader r (UnifyT t v m) where
  ask = UnifyT . lift $ E ask
  local a b =
    UnifyT $ StateT $ \s -> E ((,) <$> local a (getE . flip evalStateT s $ runUnifyT' b) <*> pure s)

instance MonadTrans (UnifyT t v) where
  lift m = UnifyT $ lift (E $ lift m)

-- | Union the equivalence classes of two terms
union :: (Monad m, Ord1 t, Ord v) => UTerm t v -> UTerm t v -> UnifyT t v m ()
union a b = UnifyT . lift $ E (equate a b)

-- | Unwrapped version of 'find'
findTerm
  :: (Monad m, Ord v)
  => Type (Either UVar v) -> UnifyT Type v m (Type (Either UVar v))
findTerm = fmap (view $ from uterm) . find . view uterm

-- | Find the representative of the term's equivalence class
find
  :: (Monad m, Ord v)
  => UTerm Type v -> UnifyT Type v m (UTerm Type v)
find a = do
  repr <- UnifyT $ lift $ E (classDesc a)
  view uterm <$>
    case repr ^. from uterm of
      TArr x y -> TArr <$> findTerm x <*> findTerm y
      TForall n s -> TForall n <$> _ (fromScope s)
      TUnit -> pure TUnit

-- | Check whether or not a 'UVar' is present in a term
occurs :: Foldable t => UVar -> UTerm t a -> Bool
occurs n t =
  n `elem` (t ^.. from uterm.folded._Left)

runUnifyT :: (Foldable t, AsVar t, Monad m) => UnifyT t v m res -> m res
runUnifyT a =
  runEquivT id combine (getE . flip evalStateT initialState $ runUnifyT' a)
  where
    initialState = UnifyState { _usDepth = 0, _usNextId = 0 }

    combine u v =
      case u ^? from uterm._Var._Left of
        Just{} -> v
        Nothing -> case v ^? from uterm._Var._Left of
          Just{} -> u
          Nothing
            | lengthOf (from uterm.folded._Left) u <
              lengthOf (from uterm.folded._Left) v -> u

-- | Convert a term into a unifiable term
unfreeze :: Functor t => t v -> UTerm t v
unfreeze = view uterm . fmap Right

-- | Attempt to convert a unifiable term into a regular term. Produces a 'Nothing' if
-- the term still contains unsolved unification variables
freeze
  :: ( Monad m
     , Ord v
     )
  => UTerm Type v
  -> UnifyT Type v m (Maybe (Type v))
freeze = fmap (fmap join . sequence) . go . view (from uterm)
  where
    go =
      traverse $ \x -> case x of
        Right a -> pure . Just $ _Var # a
        _ -> do
          r <- find (view uterm $ _Var # x)
          if nullOf (from uterm.folded._Left) r
            then freeze r
            else pure Nothing

-- | Terms that can be unified
class Unifiable term where
  -- |
  -- Checks for top-level structural equality, for example:
  --
  -- @
  -- data Ty = TyArr Ty Ty | TyVar String
  -- instance Unifiable Ty where
  --   toplevelEqual TyArr{} TyArr{} = True
  --   toplevelEqual _ _ = False
  -- @
  --
  -- Must obey the law:
  -- @forall t u. toplevelEqual t u ==> 'lengthOf' 'plate1' t == 'lengthOf' 'plate1' u@
  -- i.e. top-level equal terms must have the same number of children
  --
  -- Should obey the law:
  -- @forall t u. 'hasn't' 'plate1' t || 'hasn't' 'plate1' u ==> 'not' (toplevelEqual t u)@
  -- i.e. a term with no children should not be top-level equal to another
  -- term
  toplevelEqual :: term a -> term a -> Bool

-- | Terms from which 'variables' can be extracted
class AsVar term where
  _Var :: Prism' (term var) var

instance AsVar Type where
  _Var = _TVar

-- | Datatypes which can contain unification errors.
class AsUnificationError e term var ann | e -> term var ann where
  _OccursError :: Prism' e (UVar, UTerm term var, ann)
  _MismatchError :: Prism' e (UTerm term var, UTerm term var, ann)
  _EscapedVariables :: Prism' e ([UVar], UTerm term var, ann)

-- | Terms from which 'annotations' can be extracted
--
-- For terms that do not contain annotations, use the default instance:
--
-- @instance HasAnnotation Term ()@
class HasAnnotation term ann | term -> ann where
  annotation :: Lens' (term a) ann

  default annotation :: (ann ~ ()) => Lens' (term a) ann
  annotation = lens (const ()) const

makeLenses ''UnifyState
makeLenses ''UVar
makeWrapped ''UTerm

deeply :: Monad m => UnifyT t v m b -> UnifyT t v m b
deeply (UnifyT ma) =
  UnifyT $ do
    usDepth += 1
    b <- ma
    usDepth -= 1
    pure b

-- | Generate a fresh 'UVar'
freshVar :: Monad m => UnifyT t v m UVar
freshVar = UnifyT $ do
  count <- use usNextId
  depth <- use usDepth
  usNextId .= count+1
  pure $ UVar { _uvId = count, _uvDepth = depth }

-- | Generate a fresh 'UVar' wrapped in a 'UTerm'
fresh :: (Monad m, AsVar t) => UnifyT t v m (UTerm t v)
fresh = (from uterm._Var._Left #) <$> freshVar

unifyScopes
  :: ( Ord v
     , AsUnificationError e Type v ann
     , MonadError e m
     )
  => (Int, Scope Int Type (Either UVar v))
  -> (Int, Scope Int Type (Either UVar v))
  -> UnifyT Type v m ()
unifyScopes (mArgs, m) (nArgs, n) =
  case compare mArgs nArgs of
    EQ -> do
      vars <- replicateM mArgs $ Left <$> freshVar
      let
        m' = instantiateVars vars m ^. uterm
        n' = instantiateVars vars n ^. uterm
      unify m' n'
    LT -> do
      let remainder = nArgs - mArgs
      vars <- replicateM mArgs $ Left <$> freshVar
      let
        tvar = TVar <$> vars
        m' = instantiateVars vars m ^. uterm
        n' = applySome (TVar <$> vars) n
      unify m' (TForall remainder n' ^. uterm)
    GT -> do
      let remainder = mArgs - nArgs
      vars <- replicateM nArgs $ Left <$> freshVar
      let
        m' = applySome (TVar <$> vars) m
        n' = instantiateVars vars n ^. uterm
      unify (TForall remainder m' ^. uterm) n'

-- | Unify two terms
unify
  :: ( Ord v
     , AsUnificationError e Type v ann
     , MonadError e m
     )
  => UTerm Type v
  -> UTerm Type v
  -> UnifyT Type v m ()
unify m n = do
  a <- view (from uterm) <$> find m
  b <- view (from uterm) <$> find n
  if toplevelEqual a b
    then
      traverse_
        (uncurry . on unify $ view uterm)
        (zip (a ^.. plate1) (b ^.. plate1))
    else
      case (a ^? _Var, b ^? _Var) of
        (Just (Left a'), _)
          | occurs a' (b ^. uterm) ->
            throwError $ _OccursError # (a', b ^. uterm, a ^. annotation)
          | otherwise ->
            let
              escapes = filter ((/= _uvDepth a') . _uvDepth) $ b ^.. folded._Left
            in
              case escapes of
                [] -> (union `on` view uterm) a b
                _ ->
                  throwError $
                  _EscapedVariables # (escapes, b ^. uterm, b ^. annotation)
        (_, Just (Left b'))
          | occurs b' (a ^. uterm) ->
            throwError $ _OccursError # (b', a ^. uterm, a ^. annotation)
          | otherwise ->
            let
              escapes = filter ((/= _uvDepth b') . _uvDepth) $ a ^.. folded._Left
            in
              case escapes of
                [] -> (union `on` view uterm) a b
                _ ->
                  throwError $
                  _EscapedVariables # (escapes, a ^. uterm, a ^. annotation)
        (_, _) ->
          unless (on (==) (view uterm) a b) . throwError $
          _MismatchError # (a ^. uterm, b ^. uterm, a ^. annotation)
