module Hloc(
  jsonInit,
  getBarText,
  version
  ) where

import qualified Data.Text as T
import Data.Text(Text)
import Control.Monad

import Block
import Config

version :: Text
version = "1.1"

-- |Init text for i3bar configuration
jsonInit :: Text
jsonInit = T.concat ["{\"version\":", version, ", \"click_events\":true} ["]

-- |Turns Config into JSON list of parsed and executed blocks
getBarText :: Config -> IO Text
getBarText blocks = let
  jsons :: [IO Text]
  jsons = map blockToJson blocks
  glue :: Text -> Text -> Text
  glue s1 s2 = T.concat [s1, ", ", s2]
  surround :: Text -> Text
  surround s = T.concat ["[", s, "],"]
  in surround
     . T.init . T.init
     <$> foldr (liftM2 glue)
     (return "") jsons -- glue all blocks



