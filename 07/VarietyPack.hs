module VarietyPack where

-- 1  a. `k :: (a, b) -> a`
--    b. `k2` is a `String`, not the same as `k1` or `k3`
--    c. `k1` and `k3`

-- 2
f :: (a, b, c) -> (d, e, f) -> ((a,d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
