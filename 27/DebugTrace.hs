module DebugTrace where

import Debug.Trace (trace)

inc = (+1)

twice = inc . inc

howManyTimes = inc (trace "eval'd" (1 + 1)) + twice (trace "eval'd" (1 + 1))

howManyTimes' = let onePlusOne = (trace "eval'd" (1 + 1))
                in inc onePlusOne  + twice onePlusOne
