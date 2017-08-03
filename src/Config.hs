module Config( readConfigFile
             ) where

import Block

import Data.Text
import Text.RawString.QQ
import System.Directory
import System.IO

type Config = [Block]

readConfigFile :: FilePath -> IO Config
readConfigFile filePath =
  doesFileExist filePath >>= \e -> if not e
  then do
    hPutStrLn stderr "Config file is missing. Using default bar"
    return []

  else return [] -- TODO
