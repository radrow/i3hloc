module Config(module ConfigDefault
             , readConfigFile
             ) where

import Block
import ConfigDefault
import System.Directory
import Control.Monad
import System.IO

type Config = [Block]

getDefaultConfig :: IO Config
getDefaultConfig = return blocksDefault

readConfigFile :: FilePath -> IO Config
readConfigFile filePath =
  doesFileExist filePath >>= \e -> if not e
  then do
    hPutStrLn stderr "Config file is missing. Using default bar"
    return blocksDefault

  else do return blocksDefault -- TODO
