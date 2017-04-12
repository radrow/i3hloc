module Config where

import Data.List.Split

import Block
import Blocks.Time
import Blocks.Command
import Colors
import Blocks.Battery

myDate = newBlock {
  fullText = getTime [date]
  , color = light . light . light . light . light $ blue
  , prefix = "DATE: "
  }

myTime = newBlock {
  fullText = getTime [hour24, ":", minute, ":", second]
  , color = black
  , bgColor = Just (light cyan)
  , prefix = "TIME: "
  }

myBattery = newBlock {
  fullText = getBatteryState
  }

blocks :: [Block]
blocks = [ newBlock {fullText = staticText "LOL TEST"}
         , myBattery
         , myDate
         , myTime
         ]
