module Blocks.Bandwidth( getInterfaceUpSpeed
                       , getInterfaceDownSpeed
                       , getInterfaceState
                       , getInterfaceFullInfo
                       , getInterfaceFullInfoModified
                       ) where

import Control.Monad
import System.Directory
import System.Clock
import Data.List.Split

import Pango
import Colors

data State = Dormant | Up | Down | Missing deriving (Show, Eq)

stateToColor :: State -> Color
stateToColor state = case state of
                Up -> green
                Down -> lightGray
                Dormant -> orange
                Missing -> red

parseSpeedFile :: String -> (Integer, Integer, Rational)
parseSpeedFile s = let splitted = splitOn "\n" s in (read . head $ splitted, read $ splitted!!1, read $ splitted !! 2)

getInterfaceState :: String -> IO State
getInterfaceState interface = do
  operstateExists <- doesFileExist $ "/sys/class/net/"++interface++"/operstate"
  if operstateExists
    then do state <- filter (/= '\n') <$> readFile ("/sys/class/net/"++interface++"/operstate")
            return $ case state of
              "up" -> Up
              "down" -> Down
              "dormant" -> Dormant
              _ -> Missing
    else return Missing


getInterfaceUpSpeed :: Rational -> String -> IO String
getInterfaceUpSpeed period = getInterfaceSpeed period "tx_bytes"

getInterfaceDownSpeed :: Rational -> String -> IO String
getInterfaceDownSpeed period = getInterfaceSpeed period "rx_bytes"

getInterfaceSpeed :: Rational -> String -> String -> IO String
getInterfaceSpeed minimumPeriod file interface = do
  createDirectoryIfMissing True ("/dev/shm/"++interface)
  let
      lastStateFile = "/dev/shm/" ++ interface ++ "/" ++ file
      currentStateFile = "/sys/class/net/" ++ interface ++ "/statistics/" ++ file
      tmp = (++".tmp")

  tempFileExists <- doesFileExist lastStateFile

  currentBytes <- read <$> readFile currentStateFile
  currentTime <- toNanoSecs <$> getTime Monotonic

  unless tempFileExists $
    writeFile lastStateFile $ show currentBytes ++ "\n" ++ show currentTime ++ "\n" ++ "0"

  (lastBytes, lastTime, prevSpeed) <- parseSpeedFile <$> readFile lastStateFile

  let deltaBytes = fromIntegral (currentBytes - lastBytes)
      deltaTime = fromIntegral (currentTime - lastTime) / 10^9

      speedInBps = if deltaTime < minimumPeriod
                   then prevSpeed
                   else deltaBytes / deltaTime

  when (deltaTime >= minimumPeriod)
    (writeFile (tmp lastStateFile) ( show currentBytes
                                     ++ "\n" ++ show currentTime
                                     ++ "\n" ++ show speedInBps))
  copyFile (tmp lastStateFile) lastStateFile

  let out | speedInBps > 2 * 10^6 = show (round (speedInBps / 10^6)) ++ " MBps"
          | speedInBps > 10^3 = show (round (speedInBps / 10^3)) ++ " kBps"
          | speedInBps >= 0         = show (round speedInBps) ++ " Bps"
          | otherwise = "(loading)" -- during initialisation it produces sick values
    in return out

getInterfaceFullInfo :: Rational -> String -> IO String
getInterfaceFullInfo = getInterfaceFullInfoModified id

getInterfaceFullInfoModified :: (String -> String) -> Rational -> String -> IO String
getInterfaceFullInfoModified f period interface = do
  state <- getInterfaceState interface
  up <- getInterfaceUpSpeed period interface
  down <- getInterfaceDownSpeed period interface

  let info = case state of
               Up -> down ++ "↓ " ++ up ++ "↑"
               Down -> "down"
               Dormant -> "disconnected"
               Missing -> interface ++ " is missing!"

  return $ spanSurround "color" (show . stateToColor $ state) (f info)
