module StateT where

import Control.Monad.Trans

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s0 -> fstMap f <$> (sma s0)
    where fstMap f (x, y) = (f x, y)

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s0 -> pure (x, s0)
  (<*>) (StateT smf) (StateT sma) = StateT $ \s0 -> do
    (a, s1) <- sma s0
    (f, s2) <- smf s1
    return (f a, s2)

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (>>=) (StateT sma) f = StateT $ \s0 -> do
    (a, s1) <- sma s0
    runStateT (f a) s1

instance MonadTrans (StateT e) where
  lift m = StateT $ \s -> m >>= \a -> return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO
