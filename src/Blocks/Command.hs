module Blocks.Command ( customCommand
                      , customCommandOut
                      , customCommandErr
                      , customCommandRet
                      ) where

import qualified Data.Text as T
import Data.Text(Text, pack)
import System.Process
import System.Exit

fix :: Text -> Text
fix = T.map (\x -> if x == '\n' then ' ' else x)

customCommand :: String -> [String] -> String -> IO (ExitCode, Text, Text)
customCommand c a stdin = do
  (r, o, e)  <- readProcessWithExitCode c a stdin
  return (r, pack o, pack e)

customCommandOut :: String -> [String] -> String -> IO Text
customCommandOut c a stdin = do
  (_, o, _) <- customCommand c a stdin
  fix <$> return o

customCommandRet :: String -> [String] -> String -> IO ExitCode
customCommandRet c a stdin = do
  (r, _, _) <- customCommand c a stdin
  return r

customCommandErr :: String -> [String] -> String -> IO Text
customCommandErr c a stdin = do
  (_, _, e) <- customCommand c a stdin
  fix <$> return e

