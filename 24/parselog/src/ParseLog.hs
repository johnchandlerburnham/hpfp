--24/parselog/src/ParseLog.hs
module ParseLog where

import Data.Char (isSpace)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Scientific (toRealFloat)
import Data.Fixed (showFixed, Fixed( MkFixed ), Pico)
import Data.List (intersperse, sortBy)
import Text.Trifecta
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead (lookAhead)
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Time as T

data Log = Log [Entry] deriving Eq

data Entry = Entry Date [Line] deriving Eq
type Date = Either InvalidDate T.Day
type InvalidDate = String

data Line = Line Time Activity deriving Eq
type Time = Either InvalidTime T.TimeOfDay
type InvalidTime = String
newtype Activity = Activity String deriving (Eq, Ord)

instance Show Log where show (Log es) = concat $ intersperse "\n" $ show <$> es

instance Show Entry where
  show (Entry (Left error) lines) = 
    "# " ++ error ++ " -- InvalidDate\n" ++ concatMap show lines
  show (Entry (Right date) lines) = 
    "# " ++ show date ++ "\n" ++ concatMap show lines

instance Show Line where
  show (Line (Left error) act) = error ++ " " ++ show act ++ " -- InvalidTime\n"
  show (Line (Right time) act) = show time ++ " " ++ show act ++ "\n"

instance Show Activity where show (Activity a) = a

isNewline :: Char -> Bool
isNewline = (==) '\n'

inlineSpace :: Parser () 
inlineSpace = skipMany $ satisfy (\x -> (isSpace x) && (not $ isNewline x))

parseComment :: Parser String
parseComment = string "--" >> (many $ satisfy (not . isNewline))

endLine :: Parser Char
endLine = inlineSpace >> (skipOptional parseComment) >> newline

parseTimeOfDay :: Parser Time
parseTimeOfDay = do 
  hour <- fromIntegral <$> decimal
  minute <- option 0 $ colon >> fromIntegral <$> decimal
  second <- option 0 $ colon >> parseSecond
  let time = T.makeTimeOfDayValid hour minute second 
  let raw = (show hour) ++ ":" ++ (show minute) ++ ":" ++ (show second)
  if (isJust time) then return (Right $ fromJust $ time) else return (Left raw)

parseSecond :: Parser Pico
parseSecond = do
  a <- some $ digit
  try $ skipOptional dot
  b <- option "0" $ some $ digit
  return (read $ a ++ "." ++ b)

parseAct :: Parser Activity
parseAct = Activity <$> manyTill anyChar (try $ endLine)

parseLine :: Parser Line
parseLine = do
    time <- parseTimeOfDay 
    inlineSpace
    activity <- parseAct
    return $ Line time activity

parseDay :: Parser Date
parseDay = do
  year <- integer
  month <- char '-' >> fromIntegral <$> decimal
  day <- char '-' >> fromIntegral <$> decimal
  let date = T.fromGregorianValid year month day  
  let raw = (show year) ++ "-" ++ (show month) ++ "-" ++ (show day)
  if (isJust date) then return (Right $ fromJust $ date) else return (Left raw)

parseEntry :: Parser Entry
parseEntry = do
  char '#'
  inlineSpace
  date  <- parseDay
  endLine
  lines <- some $ parseLine
  return $ Entry date lines
 
parseLog :: Parser Log
parseLog = do
  many $ space <|> endLine 
  log <- sepEndBy parseEntry (some $ space <|> endLine)
  eof
  return $ Log log

logBimorphism :: Log -> Bool
logBimorphism log = case (parseString parseLog mempty (show log)) of
  (Success log') -> log' == log
  (Failure e) -> False


data UTCLog = UTCLog [(T.UTCTime, Activity)] deriving Show

lineToUTC :: T.Day -> Line -> Maybe (T.UTCTime, Activity)
lineToUTC _ (Line (Left _) act) = Nothing 
lineToUTC day (Line (Right time) act) = 
  Just (T.UTCTime day $ T.timeOfDayToTime time, act)

entryToUTC :: Entry -> Maybe [(T.UTCTime, Activity)]
entryToUTC (Entry (Left _) _) = Nothing
entryToUTC (Entry (Right date) lines) = Just $ mapMaybe (lineToUTC date) lines

logToUTC :: Log -> UTCLog
logToUTC (Log log) = UTCLog $ concat $ mapMaybe entryToUTC log

sortUTCLog :: UTCLog -> UTCLog
sortUTCLog (UTCLog log) = UTCLog $ sortBy (\(a, b) (c, d) -> compare a c) log

activityTotals :: UTCLog -> M.Map Activity (T.NominalDiffTime, Int)
activityTotals (UTCLog log) = go M.empty log' where
  log' = sortBy (\(a,b)(c,d)-> compare a c) log
  go map [x] = map 
  go map ((t,a):x'@(t',a'):xs) = go map' (x':xs) where
    diff = T.diffUTCTime t' t
    f (time, count) = (time + diff, count + 1)
    map' = if M.member a map then M.adjust f a map else M.insert a (diff, 1) map

avgActivityPerDay :: M.Map Activity (T.NominalDiffTime, Int) -> 
                     M.Map Activity T.NominalDiffTime
avgActivityPerDay m = M.map (\(t, c) -> t / (fromIntegral c)) m 
  
  
main' :: String -> IO ()
main' str = do
  file <- readFile str
  let log = parseString parseLog mempty file
  print $ logBimorphism <$> log
  print log
  print $ sortUTCLog <$> logToUTC <$> log
  print $ activityTotals <$> logToUTC <$> log
  print $ avgActivityPerDay <$> activityTotals <$> logToUTC <$> log
