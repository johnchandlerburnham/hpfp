--24/parsercombinators/src/ParseIPAddress.hs
module ParseIPAddress where

import Numeric
import Data.Word
import Data.Bits
import Data.Maybe (isJust, fromJust)
import Data.List
import Text.Trifecta
import Text.Parser.Char
import Text.Parser.Combinators 

data IPAddress = IPAddress Word32 deriving (Eq, Ord)

instance Show IPAddress where
  show (IPAddress ip) = a ++ "." ++ b ++ "." ++ c ++ "." ++ d where
    a = show $ (.&.) (rotateR ip 24) 255 
    b = show $ (.&.) (rotateR ip 16) 255 
    c = show $ (.&.) (rotateR ip 8) 255 
    d = show $ (.&.) ip 255
    

parseIP :: Parser IPAddress
parseIP = do
  a <- fromIntegral <$> natural
  b <- fromIntegral <$> (dot >> natural)
  c <- fromIntegral <$> (dot >> natural)
  d <- fromIntegral <$> (dot >> natural)
  let ip = makeIP a b c d
  if isJust ip 
  then return $ fromJust ip
  else unexpected "parse failed" 

makeIP :: Word32 -> Word32 -> Word32 -> Word32 -> Maybe IPAddress
makeIP a b c d = 
  if all (\x -> x >= 0 && x < 256) [a,b,c,d] 
  then Just $ IPAddress $ shift a 24 + shift b 16 + shift c 8 + d
  else Nothing

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

instance Show IPAddress6 where
  show (IPAddress6 x y) = 
    a ++ ":" ++ b ++ ":" ++ c ++ ":" ++ d ++ ":" ++ 
    e ++ ":" ++ f ++ ":" ++ g ++ ":" ++ h 
    where
      show' x = showHex x ""
      a = show' $ (.&.) (rotateR x 48) (2^16 - 1)
      b = show' $ (.&.) (rotateR x 32) (2^16 - 1)
      c = show' $ (.&.) (rotateR x 16) (2^16 - 1)
      d = show' $ (.&.) x              (2^16 - 1)
      e = show' $ (.&.) (rotateR y 48) (2^16 - 1)
      f = show' $ (.&.) (rotateR y 32) (2^16 - 1)
      g = show' $ (.&.) (rotateR y 16) (2^16 - 1)
      h = show' $ (.&.) y              (2^16 - 1)

parseIP6 :: Parser IPAddress6
parseIP6 = do
   blocks <- abbrev <$> ip6Blocks
   let ip = (fmap . fmap) (read . ("0x"++)) blocks >>= makeIP6
   if isJust ip 
   then return $ fromJust ip
   else unexpected "parse failed" 

ip6Blocks :: Parser [String]
ip6Blocks = (try $ skipOptional colon) >> sepBy (many hexDigit) colon

abbrev :: [String] -> Maybe [String]
abbrev xs = let abbrevs = elemIndices "" xs in
  case (length abbrevs) of
    0 -> Just xs
    1 -> Just $ take n xs ++ zs ++ drop (n + 1) xs where
      n = head abbrevs
      zs = replicate (8 - (length xs - 1)) "0"
    _ -> Nothing
  

makeIP6 :: [Word64] -> Maybe IPAddress6
makeIP6 xs@[a,b,c,d,e,f,g,h] = 
  if any (\x -> x < 0 && x > 65535) xs then Nothing
  else Just $ IPAddress6 (shift a 48 + shift b 32 + shift c 16 + d)
                         (shift e 48 + shift f 32 + shift g 16 + h)
makeIP6 _ = Nothing

makeIP6' :: Word64 -> Word64 -> Word64 -> Word64 -> 
           Word64 -> Word64 -> Word64 -> Word64 -> Maybe IPAddress6
makeIP6' a b c d e f g h = 
  if all (\x -> x >= 0 && x < 65536) [a,b,c,d,e,f,g,h] 
  then Just $ IPAddress6 (shift a 48 + shift b 32 + shift c 16 + d)
                         (shift e 48 + shift f 32 + shift g 16 + h)
  else Nothing

ip4to6 :: IPAddress -> IPAddress6
ip4to6 (IPAddress ip) = 
  IPAddress6 0 $ (.|.) (shift (2^16 - 1) 32) (fromIntegral ip)

