module CanICatch where

import Control.Exception

canICatch :: Exception e => e -> IO (Either ArithException ())
canICatch e = try $ throwIO e
