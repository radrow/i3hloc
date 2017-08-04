module Pango where

import Data.Text as T

import Colors

spanSurround :: Text -> Text -> Text -> Text
spanSurround par val text = T.concat ["<span ", par, "='", val, "'>", text, "</span>"]

maybeSurround :: Text -> Maybe Text -> Text -> Text
maybeSurround _ Nothing s = s
maybeSurround par (Just val) text = spanSurround par val text

colorString :: Maybe Color -> Text -> Text
colorString (Just (Color c)) s =
  T.concat["<span color='", T.toUpper c, "'>", s, "</span>"]
colorString Nothing s = s

data UnderlineMode = None | Single | Double | Error
instance Show UnderlineMode where
  show None = "none"
  show Single = "single"
  show Double = "double"
  show Error = "error"

underlineModeFromString :: String -> Maybe UnderlineMode
underlineModeFromString s = case s of
  "none" -> Just None
  "single" -> Just Single
  "double" -> Just Double
  "error" -> Just Error
  _ -> Nothing
