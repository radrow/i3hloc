module Blocks.Time where

import Control.Monad.State
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime

twoDigitFront :: String -> String
twoDigitFront s | null s        = "00"
                | length s == 1 = '0':s
                | otherwise     = take 2 s

twoDigitBack :: String -> String
twoDigitBack s | null s        = "00"
               | length s == 1 = ['0', last s]
               | otherwise     = drop (length s - 2) s

dayOfWeek :: Int -> String
dayOfWeek n = case n of
  1 -> "Monday"
  2 -> "Tuesday"
  3 -> "Wednesday"
  4 -> "Thursday"
  5 -> "Friday"
  6 -> "Saturday"
  7 -> "Sunday"

data Seq = Separator String
         | Sec
         | Minute
         | Hour24
         | Hour12
         | Day
         | Month
         | Year
         | WeekDayFull
         | WeekDayShort
         | AmPm
         | CenturyYear

extractSeq :: Seq -> LocalTime -> String
extractSeq s t =
  let timeOfDay = localTimeOfDay t
      date = localDay t
      (year, month, day) = toGregorian date
      hour = todHour timeOfDay
      minute = todMin timeOfDay
      sec = todSec timeOfDay
      wday = (\(_,_,n) -> dayOfWeek n) $ toWeekDate date
  in case s of
    Sec ->  twoDigitFront . show $ sec
    Minute ->  twoDigitFront . show $ minute
    Hour24 ->  twoDigitFront . show $ hour
    Hour12 ->  twoDigitFront . show $ hour `mod` 12
    AmPm -> if hour < 12 then "am" else "pm"
    Day ->  twoDigitFront . show $ day
    Month ->  twoDigitFront . show $ month
    Year ->  show $ year
    WeekDayFull ->  wday
    WeekDayShort -> take 3 wday
    CenturyYear -> twoDigitBack . show $ year `mod` 100
    Separator se -> se

readSeq :: Char -> Maybe Seq
readSeq s = case s of
  's' -> Just Sec
  'm' -> Just Minute
  'h' -> Just Hour12
  'H' -> Just Hour24
  'd' -> Just Day
  'M' -> Just Month
  'Y' -> Just Year
  'y' -> Just CenturyYear
  'W' -> Just WeekDayFull
  'w' -> Just WeekDayShort
  'a' -> Just AmPm
  _ -> Nothing

getTime :: [Seq] -> IO String
getTime format = do
  utcTime <- getCurrentTime
  timeZone <- getCurrentTimeZone
  let time = utcToLocalTime timeZone utcTime
  return $ flip evalState ("" :: String) $ do
    l <- forM format $ \s -> return (extractSeq s time)
    return $ concat l

