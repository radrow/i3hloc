module Blocks.Time where

import Control.Monad.State
import Data.Time.Calendar
import Data.Time.Clock
import Data.Dates( dayToDateTime
                 , day
                 , month
                 , year
                 , dateWeekDay
                 )

twoDigit :: String -> String
twoDigit s | null s        = "00"
           | length s == 1 = '0':s
           | otherwise     = s

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
         | CenturyYear

extractSeq :: Seq -> UTCTime -> String
extractSeq s t =
  let sec = diffTimeToPicoseconds (utctDayTime t) `div` (1000 * 1000 * 1000 * 1000)
      dateTime = dayToDateTime (utctDay t)
  in case s of
    Sec ->  twoDigit . show $ sec `mod` 60
    Minute ->  twoDigit . show $ sec `div` 60 `mod` 60
    Hour24 ->  twoDigit . show $ sec `div` 3600 `mod` 24
    Hour12 ->  twoDigit . show $ sec `div` 3600 `mod` 12
    Day ->  twoDigit . show $ day dateTime
    Month ->  twoDigit . show $ month dateTime
    Year ->  twoDigit . show $ year dateTime
    WeekDayFull ->  show $ dateWeekDay dateTime
    WeekDayShort -> take 3 $ show $ dateWeekDay dateTime
    CenturyYear -> twoDigit . show $ year dateTime `mod` 100
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
  _ -> Nothing

getTime :: [Seq] -> IO String
getTime format = do
  time <- getCurrentTime
  return $ flip evalState ("" :: String) $ do
    l <- forM format $ \s -> return (extractSeq s time)
    return $ concat l

