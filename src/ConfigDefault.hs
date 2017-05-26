{-#LANGUAGE OverloadedStrings#-}

module ConfigDefault(blocksDefault) where

import Data.Text as T
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
  , prefix = pack [fa FaCalendar, ' ']
  }

myTime = newBlock {
  fullText = getTime [hour24, ":", minute, ":", second]
  , color = iterate light green !! 10
  , prefix = pack [fa FaClockO, ' ']
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
  fullText = pack . show
             . (`div` (10 :: Integer))
             . (+1)
             . read -- lol
             . unpack
             . T.takeWhile (/= '.') <$> customCommandOut "light" []
  , prefix = pack [fa FaSunO, ' ']
  }

myWifi = newBlock {
  fullText = getInterfaceFullInfoModified (pack [fa FaWifi, ' '] `T.append`) 2 "wlp6s0"
  }

myEth = newBlock {
  fullText = getInterfaceFullInfoModified (pack [fa FaSitemap, ' '] `T.append`) 2 "enp7s0"
  }

myVol = newBlock {
  fullText = do
      mute <- isMute
      vol <- getVolume
      let out | mute = pack [fa FaVolumeOff, fa FaTimes]
              | vol < 15 = pack $ fa FaVolumeOff : ' ' : show vol
              | vol < 65 = pack $ fa FaVolumeDown : ' ' : show vol
              | otherwise = pack $ fa FaVolumeUp : ' ' : show vol
      return out
  , color = yellow
  }

blocksDefault :: [Block]
blocksDefault = [ myWindow
         , myLight
         , myVol
         , myWifi
         , myEth
         , myBattery
         , myDate
         , myTime
         ]
