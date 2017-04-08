module Main where

import Control.Monad
import Control.Concurrent

import Hloc

mainLoop :: IO()
mainLoop = do
  putStrLn jsonInit
  forever $ do
    statusList <- getBarText
    putStrLn statusList
    threadDelay 10000

main :: IO ()
main = mainLoop
