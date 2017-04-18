module Config where

import Web.FontAwesomeType

import Block
import Blocks.Time
import Blocks.Command
import Blocks.Battery
import Blocks.Bandwidth
import Blocks.Volume
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

myVol = newBlock {
  fullText = do
      mute <- isMute
      vol <- getVolume
      let out | mute = [fa FaVolumeOff, fa FaTimes]
              | vol < 15 = fa FaVolumeOff : ' ' : show vol
              | vol < 65 = fa FaVolumeDown : ' ' : show vol
              | otherwise = fa FaVolumeUp : ' ' : show vol
      return out
  , color = yellow
  }

blocks :: [Block]
blocks = [ myWindow
         , myLight
         , myVol
         , myWifi
         , myEth
         , myBattery
         , myDate
         , myTime
         ]
