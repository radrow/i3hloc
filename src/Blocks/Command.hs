module Blocks.Command ( customCommand
                      , customCommandOut
                      , customCommandErr
                      , customCommandRet
                      ) where

import System.Process
import System.Exit

fix :: String -> String
fix = map (\x -> if x == '\n' then ' ' else x)

customCommand :: String -> [String] -> IO (ExitCode, String, String)
customCommand c a = do
  (r, o, e)  <- readProcessWithExitCode c a ""
  return (r, o, e)

customCommandOut :: String -> [String] -> IO String
customCommandOut c a = do
  (_, o, _) <- customCommand c a
  fix <$> return o

customCommandRet :: String -> [String] -> IO ExitCode
customCommandRet c a = do
  (r, _, _) <- customCommand c a
  return r

customCommandErr :: String -> [String] -> IO String
customCommandErr c a = do
  (_, _, e) <- customCommand c a
  fix <$> return e

