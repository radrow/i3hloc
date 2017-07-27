module DisplayText where

import Data.Text
import Text.JSON

data DisplayText = AsString (IO String)
                 | AsText (IO Text)
                 | AsInteger (IO Integer)
                 | AsRational (IO Rational)

instance JSON DisplayText where
  readJSON js = case js of
    JSString s -> Ok (staticText . pack $ fromJSString s)
    JSRational _ i -> if isInt i then staticInteger (floor i)
                   else staticRational i where isInt x = x == fromInteger (round x)


staticText :: Text -> DisplayText
staticText = AsText . return

staticString :: String -> DisplayText
staticString = AsString . return

staticInteger :: Integer -> DisplayText
staticInteger = AsInteger . return

staticRational :: Rational -> DisplayText
staticRational = AsRational . return
