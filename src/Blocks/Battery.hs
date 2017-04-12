module Blocks.Battery where

import Web.FontAwesomeType
import System.Process
import Data.List.Split

import Colors
import Pango

fa :: FontAwesome -> Char
fa = fontAwesomeChar

acpiOutput :: IO String
acpiOutput = readProcess "acpi" ["-b"] ""

stateToSymbol :: String -> Int -> String
stateToSymbol state percent = case state of
  "Full" -> [fa FaPlug, ' ', fa FaHeart]
  "Charging" -> if percent < 95
               then [fa FaBolt, ' ', fa FaPlug]
               else [fa FaPlug, ' ', fa FaHeart]
  "Unknown" -> "???"
  "Discharging" -> [batterySymbol] where batterySymbol | percent >= 95 = fa FaHeart
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

parsePercent :: String -> Int
parsePercent = read . init

getBatteryState :: IO String
getBatteryState = do
  acpi <- acpiOutput
  let parsed = tail . splitOn " " . filter (/=',') $ acpi
      state = parsed !! 1
      percent = parsePercent (parsed !! 2)
      time = if state == "Full" || (percent == 100) || ':' `notElem` parsed !! 3
        then "(∞:∞)" else parsed !! 3

      color = getColorByPercent percent
      bgColor = getBgColorByPercent percent
      symbol = stateToSymbol state percent

      maybeShow (Just c) = Just (show c)
      maybeShow Nothing = Nothing
      pangoSurround = maybeSurround "bgcolor" (maybeShow bgColor) . spanSurround "color" (show color)

  return $ pangoSurround (symbol ++ " " ++ show percent ++ "% " ++ time)
