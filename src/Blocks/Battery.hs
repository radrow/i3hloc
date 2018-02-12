{-#LANGUAGE OverloadedStrings#-}

module Blocks.Battery(getBatteryState) where

import Web.FontAwesomeType
import System.Process
import qualified Data.Text as T
import Data.Text(pack, unpack, Text)

import Colors
import Pango

fa :: FontAwesome -> Char -- FontAwesome enum conversion
fa = fontAwesomeChar

acpiOutput :: IO Text
acpiOutput = pack <$> readProcess "acpi" ["-b"] ""

stateToSymbol :: Text -> Int -> Text
stateToSymbol state percent = case state of
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
  _ -> "coś się zjebało"

getColorByPercent :: Int -> Color
getColorByPercent p | p >= 99 = Color "#11FF11"
                    | p > 88 = Color "#33FF11"
                    | p > 76 = Color "#55FF11"
                    | p > 64 = Color "#77FF11"
                    | p > 42 = Color "#99FF11"
                    | p > 30 = Color "#BBDD11"
                    | p > 24 = Color "#DDBB11"
                    | p > 12 = Color "#FF8811"
                    | p > 8 = Color "#FF5511"
                    | p > 6 = Color "#FF1111"
                    | otherwise = Color "#FF2222"

getBgColorByPercent :: Int -> Maybe Color
getBgColorByPercent p = if p > 6 then Nothing
                        else Just Colors.black

parsePercent :: Text -> Int
parsePercent = read . takeWhile (/='%') . unpack

getBatteryState :: IO (Color, Maybe Color, Text)
getBatteryState = do
  acpi <- acpiOutput
  let parsed = tail . T.splitOn " " . T.filter (/=',') $ acpi
      state = parsed !! 1
      percent = parsePercent (parsed !! 2)
      time = if state == "Full"
             || (percent == 100)
             || (/=':') `T.all` (parsed !! 3)
             then "(∞:∞)"
             else parsed !! 3

      color = getColorByPercent percent
      bgColor = getBgColorByPercent percent
      symbol = stateToSymbol state percent

  return $ (color, bgColor, T.concat [symbol, " ", pack . show $ percent, "% ", time])
