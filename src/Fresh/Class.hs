{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language DefaultSignatures, TypeFamilies #-}
{-# language FlexibleInstances, UndecidableInstances #-}
module Fresh.Class where

import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)

data Stream a = Cons a (Stream a)
  deriving (Functor, Foldable, Traversable)

-- |
-- @forall s. fst (next s) /= fst (next (snd (next s)))@
class Next s a | s -> a where
  next :: s -> (a, s)

instance Next (Stream a) a where
  next (Cons a s) = (a, s)

class (Next s a, Monad m) => MonadFresh s a m | m -> s a where
  fresh :: m a
  default fresh :: (MonadFresh s a u, MonadTrans t, t u ~ m) => m a
  fresh = lift fresh

instance MonadFresh s a m => MonadFresh s a (ExceptT e m)
instance MonadFresh s a m => MonadFresh s a (ReaderT r m)
instance MonadFresh s a m => MonadFresh s a (StateT s' m)
instance (Monoid w, MonadFresh s a m) => MonadFresh s a (WriterT w m)