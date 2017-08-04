module Blocks.Volume( getVolume
                    , isMute
                    ) where

import qualified Data.Text as T
import Data.Text(Text, unpack)

import Blocks.Command(customCommandOut)

parseMute :: Text -> Bool
parseMute = (=="[off]")
            . (last . init) -- last word
            . T.splitOn " " -- split words
            . last -- last line
            . T.splitOn "\n" -- split lines

parseVol :: Text -> Int
parseVol = read -- to int
           . unpack -- to string
           . (T.tail . T.init . T.init) -- remove firs and two last chars
           . last . init . init -- pre-last word
           . T.splitOn " " -- split words
           . last -- last line
           . T.splitOn "\n" -- split lines

getVolume :: IO Int
getVolume = do
  audioStatus <- customCommandOut "amixer" ["-D", "pulse", "get", "Master"] ""
  return $ parseVol audioStatus

isMute :: IO Bool
isMute = do
  audioStatus <- customCommandOut "amixer" ["-D", "pulse", "get", "Master"] ""
  return $ parseMute audioStatus
