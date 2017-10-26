{-# LANGUAGE BangPatterns #-}
module Exercise where


-- What will :sprint output

{-
  In a source file, these expressions remain unevaluated 
  as they aren't called, so if you loaded the module into 
  ghci, :sprint x2 would return x2 = _, even though for
  let x = ['1'], :sprint x returns x = "1".
-}

-- 1: x = _
x1 = 1

-- 2: x = "1"
x2 = ['1']

-- 3: x = _
x3 = [1]

-- 4: x = 1
x4 = 1 :: Int

-- 5: x = _
f5 = \x -> x

x5 = f5 1 

-- 6: x = _
f6 :: Int -> Int
f6 = \x -> x

x6 = f6 1

-- Will printing this expression result in bottom?

-- 1. No
-- 2. Yes
-- 3. Yes
-- 4. No
-- 5. No
-- 6. No
-- 7. Yes

-- Make the expression bottom

x = undefined
y = "blah"
mainSeq = do 
 print (snd (x, seq x y))

mainBang = do 
  let s !x y = (snd (x, y))
  print $ s x y

