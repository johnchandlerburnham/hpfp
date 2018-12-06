{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           Database.SQLite.Simple       hiding (close)
import qualified Database.SQLite.Simple       as SQLite
import           Database.SQLite.Simple.Types
import           System.Environment

data User = User {
    userId        :: Integer
  , username      :: Text
  , shell         :: Text
  , homeDirectory :: Text
  , realName      :: Text
  , phone         :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
       toRow (id_, username, shell, homeDir, realName, phone)


insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
  [ "Login: ",     e username, "\t\t\t\t"
  , "Name: ",      e realName, "\n"
  , "Directory: ", e homeDir,  "\t\t\t"
  , "Shell: ",     e shell,    "\n"
  ]
  where
    e = encodeUtf8

addUser :: User -> IO ()
addUser user = do
  conn <- open "finger.db"
  execute conn insertUser user
  SQLite.close conn

mkUser :: [String] -> User
mkUser (a:b:c:d:e:f:_) =
  User (read a) (T.pack b) (T.pack c) (T.pack d) (T.pack e) (T.pack f)

main :: IO ()
main = do
  u <- getArgs
  let u' = mkUser u
  addUser u'
  return ()
