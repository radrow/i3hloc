module Colors where

import Data.Char

type ColorHex = String

newtype Color = Color ColorHex
instance Show Color where
  show c = let Color s = c in s
instance Eq Color where
  (==) (Color a) (Color b) = map toLower a == map toLower b

validateColor :: Color -> Maybe Color
validateColor (Color c) | length c /= 7 = Nothing
                        | head c /= '#' = Nothing
                        | foldl (\b char -> b ||
                                  notElem char "1234567890abcdef")
                                False (tail c) = Nothing
                        | otherwise = Just (Color c)

light :: Color -> Color
light (Color c) = Color s where
  s = map (\x -> if x `elem` "012345678abcde" then succ x
                else if x == '9' then 'a'
                else x) c

dark :: Color -> Color
dark (Color c) = Color s where
  s = map (\x -> if x `elem` "123456789bcdef" then pred x
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

