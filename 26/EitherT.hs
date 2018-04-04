--26/EitherT.hs
module EitherT where

import Control.Monad.Trans

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  -- fmap f et  = EitherT $ (fmap . fmap) f (runEitherT et)
  fmap f (EitherT mea)  = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure $ pure x
  (<*>) (EitherT mefab) (EitherT mea) = EitherT $ (fmap (<*>)) mefab <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) (EitherT ema) f = EitherT $ do
    v <- ema
    case v of
      Left e -> return (Left e)
      Right a -> runEitherT (f a)

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

swapEither :: Either e a -> Either a e
swapEither (Left e) = (Right e)
swapEither (Right a) = (Left a)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fac fbc (EitherT mab) = mab >>= (either fac fbc)
