--24/parsercombinators/src/PhoneNumber.hs
module PhoneNumber where

import Text.Trifecta
import Text.Parser.Char
import Data.Char
import Text.Parser.Combinators
import Control.Applicative

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea = read <$> count 3 digit

parseExchange :: Parser Exchange 
parseExchange = read <$> count 3 digit

parseLineNumber :: Parser LineNumber 
parseLineNumber = read <$> count 4 digit

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  let notDigit = satisfy (not . isDigit)
  skipOptional $ try (char '1' >> notDigit)
  many $ notDigit
  npa <- parseNumberingPlanArea 
  many $ notDigit 
  exc <- parseExchange
  many $ notDigit 
  lnn <- parseLineNumber 
  many $ notDigit 
  return $ PhoneNumber npa exc lnn 
  
 
  
     
