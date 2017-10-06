module ParseIPAddress where

import Data.Word
import Data.Bits
import Data.Maybe (isJust, fromJust)
import Text.Trifecta
import Text.Parser.Char
import Text.Parser.Combinators 

data IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

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

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord, Show)

parseIP6 :: Parser IPAddress6
parseIP6 = do
  a <- fromIntegral <$> hexadecimal
  b <- fromIntegral <$> (colon >> hexadecimal)
  c <- fromIntegral <$> (colon >> hexadecimal)
  d <- fromIntegral <$> (colon >> hexadecimal)
  e <- fromIntegral <$> (colon >> hexadecimal)
  f <- fromIntegral <$> (colon >> hexadecimal)
  g <- fromIntegral <$> (colon >> hexadecimal)
  h <- fromIntegral <$> (colon >> hexadecimal)
  let ip = makeIP6 a b c d e f g h
  if isJust ip 
  then return $ fromJust ip
  else unexpected "parse failed" 

makeIP6 :: Word64 -> Word64 -> Word64 -> Word64 -> 
           Word64 -> Word64 -> Word64 -> Word64 -> Maybe IPAddress6
makeIP6 a b c d e f g h = 
  if all (\x -> x >= 0 && x < 65535) [a,b,c,d,e,f,g,h] 
  then Just $ IPAddress6 (shift a 48 + shift b 32 + shift c 16 + d)
                         (shift e 48 + shift f 32 + shift g 16 + h)
  else Nothing


