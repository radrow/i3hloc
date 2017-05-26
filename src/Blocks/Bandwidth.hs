{-#LANGUAGE OverloadedStrings#-}

module Blocks.Bandwidth( getInterfaceUpSpeed
                       , getInterfaceDownSpeed
                       , getInterfaceState
                       , getInterfaceFullInfo
                       , getInterfaceFullInfoModified
                       ) where

import Control.Monad
import System.Directory
import System.Clock
import Data.Text as T

import Pango
import Colors

data State = Dormant | Up | Down | Missing deriving (Eq, Show)

stateToColor :: State -> Color
stateToColor state = case state of
                Up -> green
                Down -> lightGray
                Dormant -> orange
                Missing -> red

parseSpeedFile :: Text -> (Integer, Integer, Rational)
parseSpeedFile s = let splitted = T.splitOn "\n" s in ( read . unpack . Prelude.head $ splitted
                                                      , read . unpack $ splitted!!1
                                                      , read . unpack $ splitted !! 2)

getInterfaceState :: Text -> IO State
getInterfaceState interface = do
  let file = T.concat["/sys/class/net/", interface, "/operstate"]
  operstateExists <- doesFileExist (unpack file)
  if operstateExists
    then do state <- Prelude.filter (/= '\n') <$> readFile (unpack file)
            return $ case state of
              "up" -> Up
              "down" -> Down
              "dormant" -> Dormant
              _ -> Missing
    else return Missing

getInterfaceUpSpeed :: Rational -> Text -> IO Text
getInterfaceUpSpeed period = getInterfaceSpeed period "tx_bytes"

getInterfaceDownSpeed :: Rational -> Text -> IO Text
getInterfaceDownSpeed period = getInterfaceSpeed period "rx_bytes"

getInterfaceSpeed :: Rational -> Text -> Text -> IO Text
getInterfaceSpeed minimumPeriod file interface = do
  createDirectoryIfMissing True ("/dev/shm/" ++ unpack interface)
  let lastStateFile = T.concat ["/dev/shm/", interface, "/", file]
      currentStateFile = T.concat ["/sys/class/net/", interface, "/statistics/", file]
      tmp = (`T.append` ".tmp")

  tempFileExists <- doesFileExist $ unpack lastStateFile

  currentBytes <- read <$> readFile (unpack currentStateFile)
  currentTime <- toNanoSecs <$> getTime Monotonic

  unless tempFileExists $
    writeFile (unpack lastStateFile) $ show currentBytes ++ "\n" ++ show currentTime ++ "\n" ++ "0"

  (lastBytes, lastTime, prevSpeed) <- parseSpeedFile . pack <$>
                                     readFile (unpack lastStateFile)

  let deltaBytes = fromIntegral (currentBytes - lastBytes)
      deltaTime = fromIntegral (currentTime - lastTime) / (10^(9 :: Int))

      speedInBps = if deltaTime < minimumPeriod
                   then prevSpeed
                   else deltaBytes / deltaTime

  when (deltaTime >= minimumPeriod)
    (writeFile (unpack . tmp $ lastStateFile) ( show currentBytes
                                     ++ "\n" ++ show currentTime
                                     ++ "\n" ++ show speedInBps))
  copyFile (unpack . tmp $ lastStateFile) (unpack lastStateFile)

  let out | speedInBps > 2 * 10^(6 :: Int) = show (round (speedInBps / 10^(6 :: Int)) :: Int) ++ " MBps"
          | speedInBps > 10^(3 :: Int) = show (round (speedInBps / 10^(3 :: Int)) :: Int) ++ " kBps"
          | speedInBps >= 0         = show (round speedInBps :: Int) ++ " Bps"
          | otherwise = "(loading)" -- during initialisation it produces sick values
    in return $ pack out

getInterfaceFullInfo :: Rational -> Text -> IO Text
getInterfaceFullInfo = getInterfaceFullInfoModified id

getInterfaceFullInfoModified :: (Text -> Text) -> Rational -> Text -> IO Text
getInterfaceFullInfoModified f period interface = do
  state <- getInterfaceState interface
  up <- getInterfaceUpSpeed period interface
  down <- getInterfaceDownSpeed period interface

  let info = case state of
               Up -> T.concat [down, "↓ ", up, "↑"]
               Down -> "down"
               Dormant -> "disconnected"
               Missing -> T.concat [interface, " is missing!"]

  return $ spanSurround "color" (pack $ show . stateToColor $ state) (f info)
