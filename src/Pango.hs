module Pango where

import Data.Char

import Colors

spanSurround :: String -> String -> String -> String
spanSurround s p t = "<span "++s++"='"++p++"'>"++t++"</span>"

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

