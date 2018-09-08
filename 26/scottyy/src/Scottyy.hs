{-# LANGUAGE OverloadedStrings #-}

module Scottyy where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))
import Data.Monoid (mconcat)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    let liftStateT m = StateT $ \s -> m >>= \a -> return (a, s)
    (ActionT . (ExceptT . fmap Right) . liftReaderT . liftStateT)
      (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
