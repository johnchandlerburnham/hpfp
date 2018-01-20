--22/Ask.hs
module Ask where

import Control.Monad
import Control.Applicative

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id
