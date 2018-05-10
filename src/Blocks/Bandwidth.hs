{-#LANGUAGE ScopedTypeVariables#-}
module Blocks.Bandwidth( getInterfaceUpSpeed
                       , getInterfaceDownSpeed
                       , getInterfaceState
                       , getInterfaceFullInfo
                       , getInterfaceFullInfoModified
                       ) where

import Control.Monad
import Control.Exception
import System.Directory
import System.IO
import System.Clock
import Data.Text as T

import Pango
import Colors

data State = Dormant | Up | Down | Unknown | Missing deriving (Eq, Show)

stateToColor :: State -> Color
stateToColor state = case state of
                Up -> green
                Down -> lightGray
                Dormant -> orange
                Unknown -> white
                Missing -> red

parseSpeedFile :: Text -> (Integer, Integer, Double)
parseSpeedFile s = let splitted = T.splitOn "\n" s in ( read . unpack . Prelude.head $ splitted
                                                      , read . unpack $ splitted!!1
                                                      , read . unpack $ splitted !! 2)

getInterfaceState :: String -> IO State
getInterfaceState interface = do
  let file = Prelude.concat["/sys/class/net/", interface, "/operstate"]
  operstateExists <- doesFileExist file
  if operstateExists
    then do state <- Prelude.filter (/= '\n') <$> readFile file
            return $ case state of
              "up" -> Up
              "down" -> Down
              "dormant" -> Dormant
              "unknown" -> Unknown
              _ -> Missing
    else return Missing

getInterfaceUpSpeed :: Double -> String -> IO Text
getInterfaceUpSpeed period = getInterfaceSpeed period "tx_bytes"

getInterfaceDownSpeed :: Double -> String -> IO Text
getInterfaceDownSpeed period = getInterfaceSpeed period "rx_bytes"

getInterfaceSpeed :: Double -> String -> String -> IO Text
getInterfaceSpeed minimumPeriod file interface = do
  -- creating directory in /dev/shm
  createDirectoryIfMissing True ("/dev/shm/" ++ interface)

  -- path to managed file, path to read file
  let lastStateFile = Prelude.concat ["/dev/shm/", interface, "/", file]
      currentStateFile = Prelude.concat ["/sys/class/net/", interface, "/statistics/", file]

  -- read current state
  currentBytes <- read <$> readFile currentStateFile
  currentTime <- toNanoSecs <$> getTime Monotonic

  (lastStateDataE :: Either IOException String) <- try $ readFile lastStateFile
  case lastStateDataE of
    Left _ -> do
      writeFile lastStateFile (show currentBytes ++ "\n" ++ show currentTime ++ "\n" ++ "0")
      return "loading"

    Right lastStateData -> do
      let (lastBytes, lastTime, prevSpeed) = parseSpeedFile . pack $ lastStateData
          deltaBytes = fromIntegral (currentBytes - lastBytes)
          deltaTime = fromIntegral (currentTime - lastTime) / (10^(9 :: Int))

          speedInBps = if deltaTime < minimumPeriod
                       then prevSpeed
                       else deltaBytes / deltaTime

      when (deltaTime >= minimumPeriod)
        (writeFile lastStateFile ( show currentBytes
                                   ++ "\n" ++ show currentTime
                                   ++ "\n" ++ show speedInBps))

      let out | speedInBps > 2 * 10^(6 :: Int) = show (round (speedInBps / 10^(6 :: Int)) :: Int) ++ " MBps"
              | speedInBps > 10^(3 :: Int)     = show (round (speedInBps / 10^(3 :: Int)) :: Int) ++ " kBps"
              | speedInBps >= 0                = show (round speedInBps :: Int) ++ " Bps"
              | otherwise                      = "(loading)" -- during initialisation it produces sick values
        in return $ pack out

getInterfaceFullInfo :: Double -> String -> IO (Color, Text)
getInterfaceFullInfo = getInterfaceFullInfoModified id

getInterfaceFullInfoModified :: (Text -> Text) -> Double -> String -> IO (Color, Text)
getInterfaceFullInfoModified f period interface = do
  state <- getInterfaceState interface
  info <- case state of
           Up -> do
             up <- getInterfaceUpSpeed period interface
             down <- getInterfaceDownSpeed period interface
             return $ T.concat [down, "↓ ", up, "↑"]
           Down -> return "down"
           Dormant -> return "disconnected"
           -- Unknown -> T.concat ["? ", down, "↓ ", up, "↑", " ?"]
           Unknown -> return "?"
           Missing -> return $ T.concat [pack interface, " is missing!"]

  return (stateToColor state, f info)
