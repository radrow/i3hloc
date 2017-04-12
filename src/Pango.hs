module Pango where

import Data.Char

import Colors

spanSurround :: String -> String -> String -> String
spanSurround par val text = "<span " ++ par ++ "='" ++ val ++ "'>" ++ text ++ "</span>"

maybeSurround :: String -> Maybe String -> String -> String
maybeSurround _ Nothing s = s
maybeSurround par (Just val) text = spanSurround par val text

colorString :: Maybe Color -> String -> String
colorString (Just (Color c)) s =
  "<span color='" ++ map toUpper c ++ "'>" ++ s ++ "</span>"
colorString Nothing s = s

data UnderlineMode = None | Single | Double | Error
instance Show UnderlineMode where
  show None = "none"
  show Single = "single"
  show Double = "double"
  show Error = "error"

