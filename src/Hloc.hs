module Hloc(
  prefix,
  getBarText
  ) where

version :: String
version = "1"

prefix :: String
prefix = "{\""++ version ++ "\":1, \"click_events\":true} ["


getBarText :: IO [String]
getBarText = return ["TEST", "teścik", "lol"]

