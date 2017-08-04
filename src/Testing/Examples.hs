{-#LANGUAGE QuasiQuotes#-}

module Testing.Examples where

import Text.Trifecta
import Data.Text
import Block
import Parsing.ConfigParser
import Text.RawString.QQ

intro :: String
intro = [r|
# This is example config file. Some variables are unset you may add them yourself.
# Everything is statically weak typed (eg. you may pass bool value to string type,
# but not use string type where program expects int)
|]

exampleConfig :: Text
exampleConfig = [r|

{wifi,eth}

[wifi]
string type = bandwidth
string interface = wlp6s0
float period = 2

[eth]
string type = bandwidth
string interface = enp7s0
float period = 2

[battery]
string type = battery

|]


fixx :: Result [Block] -> [Block]
fixx (Success s) = s
fixx (Failure f) = [errorBlock (show f)]

ios :: IO String
ios = Prelude.head . Prelude.map (unpack<$>) . Prelude.map blockToJson . fixx $ parseString parseConfigFile mempty (unpack exampleConfig)
