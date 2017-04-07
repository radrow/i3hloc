module Pango where

import Data.Char

import Colors

colorString :: Color -> String -> String
colorString (Color c) s =
  "<span color='" ++ map toUpper c ++ "'>" ++ s ++ "</span>"
