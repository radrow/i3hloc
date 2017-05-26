{-#LANGUAGE OverloadedStrings#-}

module Colors where

import Data.Text as T
import Text.Regex.Posix

type ColorHex = Text

newtype Color = Color ColorHex
instance Show Color where
  show c = let Color s = c in unpack s

instance Eq Color where
  (==) (Color a) (Color b) = T.toLower a == T.toLower b

validateColor :: Color -> Maybe Color
validateColor (Color c) = if unpack c =~ ("#(([0-9]|[a-f]){6})" :: String) then Just . Color $ c else Nothing

light :: Color -> Color
light (Color c) = Color s where
  s = T.map (\x -> if x `elem` ("012345678abcde" :: String) then succ x
                else if x == '9' then 'a'
                else x) c

dark :: Color -> Color
dark (Color c) = Color s where
  s = T.map (\x -> if x `elem` ("123456789bcdef" :: String) then pred x
                else if x == 'a' then '9'
                else x) c


black :: Color
white :: Color
red :: Color
green :: Color
blue :: Color
yellow :: Color
cyan :: Color
magenta :: Color
orange :: Color
purple :: Color
pink :: Color
lightGray :: Color
darkGray :: Color

black = Color "#000000"
white = Color "#ffffff"

red = Color "#ff0000"
green = Color "#00ff00"
blue = Color "#0000ff"

yellow = Color "#ffff00"
magenta = Color "#ff00ff"
cyan = Color "#00ffff"

orange = Color "#ffa500"
purple = Color "#800080"
pink = Color "#ffc0cb"

lightGray = Color "#aaaaaa"
darkGray = Color "#666666"

