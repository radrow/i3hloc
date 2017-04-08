module Hloc(
  jsonInit,
  getBarText,
  version
  ) where

import Control.Monad

import Config
import Block

version :: String
version = "1"

jsonInit :: String
jsonInit = "{\"version\":"++version++", \"click_events\":true} ["

getBarText :: IO String
getBarText = let
  jsons = map blockToJson blocks
  glue s1 s2 = (", " ++ s1) ++ s2
  surround s = "[" ++ s ++ "],"
  in surround . tail . tail <$> foldr (liftM2 glue) (return "") jsons



