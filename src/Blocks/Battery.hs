{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiWayIf#-}

module Blocks.Battery( getBatteryState
                     , getAcpiBatteryState) where

import Data.Time.Clock.POSIX
import Web.FontAwesomeType
import System.Process
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text(pack, unpack, Text)
import Control.Monad.Trans.Except
import Control.Monad.Trans
import Control.Exception
import System.IO

import Colors
import Pango

fa :: FontAwesome -> Char -- FontAwesome enum conversion
fa = fontAwesomeChar

acpiOutput :: IO Text
acpiOutput = pack <$> readProcess "acpi" ["-b"] ""

stateToSymbol :: Text -> Int -> Text
stateToSymbol state percent = case T.filter (`notElem` [' ', '\n']) state of
  "Full" -> pack [fa FaPlug, ' ', fa FaHeart]
  "Charging" -> if percent < 95
               then pack [fa FaBolt, ' ', fa FaPlug]
               else pack [fa FaPlug, ' ', fa FaHeart]
  "Unknown" -> "???"
  "Discharging" -> pack [batterySymbol]
    where batterySymbol | percent >= 95 = fa FaHeart
                        | percent > 80 = fa FaBattery4
                        | percent > 60 = fa FaBattery3
                        | percent > 40 = fa FaBattery2
                        | percent > 20 = fa FaBattery1
                        | percent > 10 = fa FaBattery0
                        | otherwise = fa FaHeartbeat
  lol -> lol

getColorByPercent :: Int -> Int -> Color
getColorByPercent time p | p >= 99 = Color "#11FF11"
                         | p > 88 = Color "#33FF11"
                         | p > 76 = Color "#55FF11"
                         | p > 64 = Color "#77FF11"
                         | p > 42 = Color "#99FF11"
                         | p > 30 = Color "#BBDD11"
                         | p > 24 = Color "#DDBB11"
                         | p > 12 = Color "#FF8811"
                         | p > 8 = Color "#FF5511"
                         | p > 6 = if even time then Color "#FF1111"
                                   else black
                         | otherwise = if even time then Color "#FF2222"
                                       else black

getBgColorByPercent :: Int -> Int -> Maybe Color
getBgColorByPercent time p = if | p > 8 -> Nothing
                                | otherwise ->
                                  if odd time then Just red
                                  else Nothing

parsePercent :: Text -> Int
parsePercent = read . takeWhile (/='%') . unpack

getBatteryState :: String -> IO (Color, Maybe Color, Text)
getBatteryState bat = do
  let dir = "/sys/class/power_supply/" ++ bat
      alarmF = dir ++ "/alarm"
      capacityF = dir ++ "/capacity"
      statusF = dir ++ "/status"
  (res :: Either IOException (Color, Maybe Color, Text)) <- runExceptT $ do
    alarm <- (=="0") <$> ExceptT (try $ readFile alarmF)
    capacity <- read <$> ExceptT (try $ readFile capacityF)
    status <- ExceptT $ try $ T.readFile statusF
    time <- liftIO $ fmap round getPOSIXTime
    if alarm
      then return (red, Nothing, "ALARM")
      else return ( getColorByPercent time capacity
                  , getBgColorByPercent time capacity
                  , T.concat [stateToSymbol status capacity, " ", pack $ show capacity, "%"])
  case res of
    Left e -> do
      hPutStrLn stderr $ show e
      return (red, Nothing, "Incompatible files in /sys/class. Try acpi variant")
    Right r -> return r

getAcpiBatteryState :: IO (Color, Maybe Color, Text)
getAcpiBatteryState = do
  acpi <- acpiOutput
  realtime :: Int <- fmap round getPOSIXTime
  let parsed = tail . T.splitOn " " . T.filter (/=',') $ acpi
      state = parsed !! 1
      percent = parsePercent (parsed !! 2)
      time = if state == "Unknown" then ""
             else if state == "Full"
             || (percent == 100)
             || (/=':') `T.all` (parsed !! 3)
             then "(∞:∞)"
             else parsed !! 3

      color :: Color
      color = getColorByPercent realtime percent
      bgColor = getBgColorByPercent realtime percent
      symbol = stateToSymbol state percent

  return $ (color, bgColor, T.concat [symbol, " ", pack . show $ percent, "% ", time])
