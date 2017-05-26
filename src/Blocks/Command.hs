module Blocks.Command ( customCommand
                      , customCommandOut
                      , customCommandErr
                      , customCommandRet
                      ) where

import qualified Data.Text as T
import Data.Text(Text, pack, unpack)
import System.Process
import System.Exit

fix :: Text -> Text
fix = T.map (\x -> if x == '\n' then ' ' else x)

customCommand :: String -> [String] -> IO (ExitCode, Text, Text)
customCommand c a = do
  (r, o, e)  <- readProcessWithExitCode c a ""
  return (r, pack o, pack e)

customCommandOut :: String -> [String] -> IO Text
customCommandOut c a = do
  (_, o, _) <- customCommand c a
  fix <$> return o

customCommandRet :: String -> [String] -> IO ExitCode
customCommandRet c a = do
  (r, _, _) <- customCommand c a
  return r

customCommandErr :: String -> [String] -> IO Text
customCommandErr c a = do
  (_, _, e) <- customCommand c a
  fix <$> return e

