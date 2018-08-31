{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad           (replicateM)
import Control.Monad.IO.Class  (liftIO)
import Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import Network.URI             (URI, parseURI)
import Web.Scotty

import qualified Data.ByteString.Char8  as BC
import qualified Data.Text.Lazy         as TL
import qualified Database.Redis         as R
import qualified System.Random          as SR

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs !! randomDigit)

shortGen :: IO [Char]
shortGen = replicateM 7 $ randomElement alphaNum

saveURI :: R.Connection -> BC.ByteString -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri = R.runRedis conn $ R.set shortURI uri

getURI :: R.Connection -> BC.ByteString 
       -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

linkShort :: String -> String
linkShort short = concat
  [ "<a href=\""
  , short
  , "\">Copy and paste your short URL</a>"
  ]

shortCreated :: Show a => a -> String -> TL.Text
shortCreated resp shawty = TL.concat 
  [ TL.pack $ show resp
  , " short is: "
  , TL.pack $ linkShort shawty
  ]

shortAintURI :: TL.Text -> TL.Text
shortAintURI uri = TL.concat
  [ uri
  , " wasn't a url,"
  , " did you forget http://?"
  ]

shortFound :: TL.Text -> TL.Text
shortFound tbs = TL.concat
  [ "<a href=\""
  , tbs, "\">"
  , tbs, "</a>"
  ]

shortDuplicated :: TL.Text -> TL.Text -> TL.Text
shortDuplicated short uri =
  TL.concat [ "ShortURI "
            , short 
            , " already exists, from link: "
            , uri 
            , " ! Please try again."
            ]

app :: R.Connection -> ScottyM ()
app rConn = do
  get "/" $ do
    uri <- param "uri"
    let parsedURI = parseURI (TL.unpack uri)
    case parsedURI of 
      Nothing -> text $ shortAintURI uri
      Just _ -> do
        shortURI <- liftIO shortGen
        let shortURI' = BC.pack shortURI
            uri'      = encodeUtf8 $ TL.toStrict uri
        uriExists <- liftIO $ getURI rConn shortURI'
        case uriExists of
          Left reply -> text $ TL.pack $ show reply
          Right mbBS -> case mbBS of
            Just bs -> text $ shortDuplicated (TL.pack shortURI) uri
            Nothing -> do
              resp <- liftIO $ saveURI rConn shortURI' uri'
              html $ shortCreated resp shortURI

  get "/:short" $ do
    short <- param "short"
    uri <- liftIO $ getURI rConn short
    case uri of
      Left reply -> text $ TL.pack $ show reply
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html $ shortFound $ TL.fromStrict $ decodeUtf8 bs

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)

