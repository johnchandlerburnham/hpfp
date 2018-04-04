--24/parsercombinators/src/SemVer.hs
module SemVer where

import Text.Trifecta
import Text.Parser.Char
import Text.Parser.Combinators
import Control.Monad
import Control.Applicative

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.'
  minor <- integer 
  char '.'
  patch <- integer 
  release <- parseAnnotation '-' 
  metadata <- parseAnnotation '+' 
  return $ SemVer major minor patch release metadata

parseAnnotation :: Char -> Parser [NumberOrString]
parseAnnotation head = try ((char head) >> sepBy parseNOS dot) <|> return []

parseNOS :: Parser NumberOrString
parseNOS = (NOSI <$> integer) <|> (NOSS <$> (many alphaNum))


  
