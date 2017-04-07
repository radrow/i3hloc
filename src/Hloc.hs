module Hloc(
  prefix,
  getBarText,
  version
  ) where

import Control.Monad

import Colors
import Pango
import Blocks.Date

data Block = Block {
  color :: Color,
  fullText :: IO String
                }

mkBlock :: IO String -> Block
mkBlock s = Block {color = white, fullText = s}

mkColorBlock :: Color -> IO String -> Block
mkColorBlock c s = Block {color = c, fullText = s}

version :: String
version = "1"

prefix :: String
prefix = "{\"version\":"++version++", \"click_events\":true} ["

getBarText :: IO String
getBarText = let
  jsons = map blockToJson blocks
  glue s1 s2 = (", " ++ s1) ++ s2
  surround s = "[" ++ s ++ "],"
  in surround . tail . tail <$> foldr (liftM2 glue) (return "") jsons

blockToJson :: Block -> IO String
blockToJson b = (\ t -> "{\"markup\":\"pango\", \"full_text\": \""
                       ++ colorString (color b) t ++ "\"}") <$> (fullText b)


blocks :: [Block]
blocks = [ mkBlock (return "TEST, 2017!!!")
         , mkColorBlock Colors.red (return "chuj")
         , mkColorBlock Colors.green (getDate "%Y %M %d")
         ];
