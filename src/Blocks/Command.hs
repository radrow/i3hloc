module Blocks.Command ( customCommand
                    , customCommandErr
                    ) where

import System.Process

fix :: String -> String
fix = map (\x -> if x == '\n' then ' ' else x)

customCommand :: String -> [String] -> IO String
customCommand c a = fix <$> readProcess c a ""

customCommandErr :: String -> [String] -> IO String
customCommandErr c a = do
  (_, _, e) <- readProcessWithExitCode c a ""
  fix <$> return e
