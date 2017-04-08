module Config where

import Block
import Blocks.Time
import Colors

myDate = newBlock {
  fullText = getTime [date]
  , color = light blue
  , prefix = "DATE: "
  }

myTime = newBlock {
  fullText = getTime [clock24]
  , color = black
  , bgColor = Just (light cyan)
  , prefix = "TIME: "
  }

blocks :: [Block]
blocks = [ newBlock {fullText = staticText "LOL TEST"}
         , myDate
         , myTime
         ]
