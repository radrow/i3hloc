module Blocks.Volume( getVolume
                    , isMute
                    ) where

import Data.List.Split

import Blocks.Command(customCommandOut)

parseMute :: String -> Bool
parseMute status = (=="[off]")
                . (last . init) -- last word
                . splitOn " " -- split words
                . last -- last line
                . splitOn "\n" -- split lines
                $ status

parseVol :: String -> Int
parseVol status = read -- to int
                . (tail . init . init) -- remove firs and two last chars
                . (last . init . init) -- pre-last word
                . splitOn " " -- split words
                . last -- last line
                . splitOn "\n" -- split lines
                $ status

getVolume :: IO Int
getVolume = do
  audioStatus <- customCommandOut "amixer" ["-D", "pulse", "get", "Master", "|", "grep", "'Front Left:'", "|", "awk", "'{print $6}'"]
  return $ parseVol audioStatus

isMute :: IO Bool
isMute = do
  audioStatus <- customCommandOut "amixer" ["-D", "pulse", "get", "Master", "|", "grep", "'Front Left:'", "|", "awk", "'{print $6}'"]
  return $ parseMute audioStatus
