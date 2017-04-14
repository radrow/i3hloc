module Config where

import Web.FontAwesomeType

import Block
import Blocks.Time
import Blocks.Command
import Blocks.Battery
import Blocks.Bandwidth
import Colors
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
  fullText = customCommandOut "xdotool" ["getactivewindow", "getwindowname"]
  , color = yellow
  , underline = Error
  }

myLight = newBlock {
  fullText = show
             . (`div` (10 :: Integer))
             . (+1)
             . read -- lol
             . takeWhile (/= '.') <$> customCommandOut "light" []
  , prefix = [fa FaSunO, ' ']
  }

myWifi = newBlock {
  fullText = getInterfaceFullInfoModified ([fa FaWifi, ' ']++) 2 "wlp6s0"
  }

myEth = newBlock {
  fullText = getInterfaceFullInfoModified ([fa FaSitemap, ' ']++) 2 "enp7s0"
  }

blocks :: [Block]
blocks = [ myWindow
         , myLight
         , myWifi
         , myEth
         , myBattery
         , myDate
         , myTime
         ]
