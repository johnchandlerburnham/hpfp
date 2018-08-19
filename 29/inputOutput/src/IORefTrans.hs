module IORefTrans where

import Control.Monad (replicateM)
import System.Random (randomRIO)

gimmeShelter :: Bool -> IO [Int]
gimmeShelter True = replicateM 10 (randomRIO (0, 10))
gimmeShelter False = pure [0]


