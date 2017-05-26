{-#LANGUAGE OverloadedStrings#-}

module Blocks.Time where

import Data.Text as T
import System.Process

getTime :: [Text] -> IO Text
getTime sequences = T.init . pack <$> readProcess "date" [unpack args] ""
  where args = "+" `T.append` T.concat sequences

type Seq = Text

weekdayNameShort :: Seq
weekdayNameFull :: Seq
weekdayNameShort = "%a"
weekdayNameFull = "%A"

monthNameShort :: Seq
monthNameFull :: Seq
monthNameShort = "%b"
monthNameFull = "%B"

dateAndTime :: Seq
dateAndTime = "%c"

century :: Seq
century = "%C"

dayOfMonth :: Seq
dayOfMonth = "%d"

date :: Seq
date = "%D"
fullDate :: Seq
fullDate = "%F"

centuryYear :: Seq
centuryYear = "%y"

hour24 :: Seq
hour12 :: Seq
hour24 = "%H"
hour12 = "%I"

dayOfYear :: Seq
dayOfYear = "%j"

month :: Seq
month = "%m"

minute :: Seq
minute = "%M"

nanosec :: Seq
nanosec = "%N"

ampm :: Seq
ampm = "%p"

clock12 :: Seq
clock24 :: Seq
clock12 = "%r"
clock24 = "%R"

second :: Seq
second = "%S"

weekdayMon :: Seq
weekdaySun :: Seq
weekdayMon = "%u"
weekdaySun = "%w"

weekNum :: Seq
weekNum = "%V"

year :: Seq
year = "%Y"

timezone :: Seq
timezone = "%:z"
