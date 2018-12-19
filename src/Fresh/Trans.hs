{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language UndecidableInstances #-}
module Fresh.Trans where

import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), StateT, state, evalStateT)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Writer (MonadWriter(..))
import Data.Functor.Identity (Identity(..))

import Fresh.Class

newtype FreshT s v m a
  = FreshT
  { unFreshT :: StateT s m a
  } deriving (Functor, Applicative, Monad)

type Fresh s v = FreshT s v Identity

runFreshT :: Monad m => FreshT s v m a -> s -> m a
runFreshT (FreshT m) s = evalStateT m s

runFresh :: Fresh s v a -> s -> a
runFresh m = runIdentity . runFreshT m

instance MonadTrans (FreshT s v) where
  lift = FreshT . lift
instance MonadIO m => MonadIO (FreshT s v m) where
  liftIO = lift . liftIO
instance (Next s v, Monad m) => MonadFresh s v (FreshT s v m) where
  fresh = FreshT $ state next
instance MonadError e m => MonadError e (FreshT e v m) where
  throwError = FreshT . throwError
  catchError ma f = FreshT $ catchError (unFreshT ma) (unFreshT . f)
instance MonadReader r m => MonadReader r (FreshT s v m) where
  ask = FreshT ask
  local f = FreshT . local f . unFreshT
instance MonadState s m => MonadState s (FreshT s' v m) where
  get = FreshT $ lift get
  put = FreshT . lift . put
instance MonadWriter w m => MonadWriter w (FreshT s v m) where
  tell = FreshT . tell
  listen = FreshT . listen . unFreshT
  pass = FreshT . pass . unFreshT