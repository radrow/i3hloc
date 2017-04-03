module Main where

import Control.Monad
import Control.Concurrent

import Hloc

listToString :: [String] -> String
listToString = surroundList . surroundText where
  surroundList :: [String] -> String
  surroundList l = "[" ++ foldr (++) "]," l

  surroundText :: [String] -> [String]
  surroundText = reverse . sur [] where
    sur :: [String] -> [String] -> [String]
    sur acc l = case l of
      [] -> acc
      [t] -> ("{" ++ t ++ "}") : acc
      t:tl -> sur (("{" ++ t ++ "},") : acc) tl


mainLoop :: IO()
mainLoop = do
  putStrLn prefix
  forever $ do
    statusList <- getBarText
    putStrLn $ listToString statusList
    threadDelay 1000000

main :: IO ()
main = mainLoop
