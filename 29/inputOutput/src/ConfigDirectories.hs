module ConfigDirectories where

import qualified DataIni as I
import System.IO
import System.Directory
import System.FilePath.Posix
import Text.Trifecta
import qualified Data.Map as Map

type ConfigMap = Map.Map FilePath (Maybe I.Config)

parseConfigDir :: FilePath -> IO ConfigMap
parseConfigDir dir = do
  files <- filter (\x -> takeExtension x == ".ini") <$> getDirectoryContents dir
  inis <- traverse (parseFromFile I.parseIni) files
  return $ Map.fromList $ zip files inis

