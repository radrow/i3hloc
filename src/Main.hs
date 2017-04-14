module Main where

import Control.Monad
import Control.Concurrent
import System.Clock

import Hloc

-- period over 1 milion with current second display is reasonless
period :: Integer
period = 1000000

repeats :: Int
repeats = 20

for :: Int -> IO a -> IO ()
for i f = unless (i == 0) $
          f >> for (i-1) f

mainLoop :: IO()
mainLoop = do
  putStrLn jsonInit
  forever $ do
    preTime <- (`div`1000) . toNanoSecs <$> getTime Monotonic
    statusList <- getBarText
    for repeats (putStrLn statusList)
    postTime <- (`div`1000) . toNanoSecs <$> getTime Monotonic

    let deltaTime = max (postTime - preTime) 10
    threadDelay . fromIntegral $ (period - deltaTime)

main :: IO ()
main = mainLoop
