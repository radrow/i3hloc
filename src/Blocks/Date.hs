module Blocks.Date(getDate) where

import System.Process

getDate :: String -> IO String
getDate args = init <$> readProcess "date" ['+':args] ""
