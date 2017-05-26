{-#LANGUAGE OverloadedStrings#-}

module Hloc(
  jsonInit,
  getBarText,
  version
  ) where

import qualified Data.Text as T
import Data.Text(Text, pack, unpack)
import Control.Monad

import Block

version :: Text
version = "1"

jsonInit :: Text
jsonInit = T.concat ["{\"version\":", version, ", \"click_events\":true} ["]

getBarText :: [Block] -> IO Text
getBarText blocks = let
  jsons :: [IO Text]
  jsons = map blockToJson blocks
  glue :: Text -> Text -> Text
  glue s1 s2 = T.concat [s1, ", ", s2]
  surround :: Text -> Text
  surround s = T.concat ["[", s, "],"]
  in surround
     . T.init . T.init
     -- . T.tail . T.tail -- remove ", "
     <$> foldr (liftM2 glue)
     (return "") jsons -- glue all blocks



