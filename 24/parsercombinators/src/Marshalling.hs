{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Marshalling where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy as LBS
import Data.ByteString as BS
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ
import Data.Scientific (floatingOrInteger)

sectionJson :: BS.ByteString
sectionJson = [r|
{ "section": {"host":"wikipedia.org"},
  "whatisit":{"red": "intoothandclaw"}
}
|]

data TestData = TestData { section :: Host, what :: Color } deriving (Eq, Show)

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _ = fail "Expected an object for Host"

newtype Host = Host String deriving (Eq, Show)

type Annotation = String

data Color = Red Annotation | Blue Annotation | Yellow Annotation 
    deriving (Eq, Show)

instance FromJSON Color where
  parseJSON (Object v) = (Red <$> v .: "red") 
                     <|> (Blue <$> v .: "blue")
                     <|> (Yellow <$> v .: "yellow")
  parseJSON _ = fail "Expected an object for Color"

data NumberOrString = Numba Integer | Stringy Text deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) = 
    case floatingOrInteger i of 
      (Left _) -> fail "Must be integral number"
      (Right integer) -> return $ Numba integer
  parseJSON (String s) = return $ Stringy s
  parseJSON _ = fail "NumberOrString must be number or string"

dec :: LBS.ByteString -> Maybe NumberOrString
dec = decode

eitherDec :: LBS.ByteString -> Either String NumberOrString
eitherDec = eitherDecode

main = do 
  let d :: Maybe Value
      d = decodeStrict sectionJson
  print d
  print $ dec "blah"
  print $ eitherDec "blah"
