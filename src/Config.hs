module Config where

import Web.FontAwesomeType

import Block
import Blocks.Time
import Blocks.Command
import Colors
import Blocks.Battery
import Pango

fa = fontAwesomeChar

myDate = newBlock {
  fullText = getTime [dayOfMonth, "-", month, "-", year]
  , color = light . light . light . light . light $ blue
  , prefix = [fa FaCalendar, ' ']
  }

myTime = newBlock {
  fullText = getTime [hour24, ":", minute, ":", second]
  , color = iterate light green !! 10
  , prefix = [fa FaClockO, ' ']
  }

myBattery = newBlock {
  fullText = getBatteryState
  }

myWindow = newBlock {
  fullText = customCommand "xdotool" ["getactivewindow", "getwindowname"]
  , color = yellow
  , underline = Error
  }

myLight = newBlock {
  fullText = show
             . (`div` (10 :: Integer))
             . (+1)
             . read -- lol
             . takeWhile (/= '.') <$> customCommand "light" []
  , prefix = [fa FaSunO, ' ']
  }

blocks :: [Block]
blocks = [ myWindow
         , myLight
         , myBattery
         , myDate
         , myTime
         ]
