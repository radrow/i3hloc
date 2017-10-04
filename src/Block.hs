module Block( staticText
            , makeBlock
            , blockToJson
            , Block( underline
                   , color
                   , bgColor
                   , prefix
                   , suffix
                   , displayText
                   )
            , errorBlock
            , getDisplayedText
            ) where

import Control.Exception

import Colors
import Pango hiding (Error)
import DisplayText

import qualified Data.Text as T
import Data.Text(Text, pack)
import System.IO(stderr, hPutStrLn)

-- |Block is responsible for displaying text from selected module with
-- |certain modifications applied
data Block = Block
  { color :: Color
  , bgColor :: Maybe Color
  , displayText :: DisplayText
  , prefix :: Text
  , suffix :: Text
  , underline :: UnderlineMode
  }

-- |Just displays text
staticText :: String -> DisplayText
staticText = display . return

-- |Default Block creator with undefined displayText. It may not be exported.
newBlock :: Block
newBlock = Block { displayText = undefined
                 , color = Colors.white
                 , bgColor = Nothing
                 , prefix = ""
                 , suffix = ""
                 , underline = None
                 }

-- |Creates default block using given DisplayText
makeBlock :: DisplayText -> Block
makeBlock d = newBlock{displayText = d}

-- |Extracts unmodified Text to be displayed from Block
getDisplayedText :: Block -> IO Text
getDisplayedText b =
  let (DisplayText d) = displayText b in d

-- |Default block that displays error message
errorBlock :: String -> Block
errorBlock errorText = newBlock
  { displayText = staticText errorText
  , color = red
  , prefix = "["
  , suffix = "]"
  }

-- |Used when Block throws an exception. Prints to stderr message of exception
-- |and returns error message
handleBlockException :: SomeException -> IO Text
handleBlockException e = do
  hPutStrLn stderr ("\n" ++ displayException e ++ "\n")
  return errMsg where errMsg = spanSurround "color" "red" "[BLOCK ERROR (e)]"

-- |Forces evaluation of block's displayText, to face all exceptions
forceDisplayEvaluation :: IO Text -> IO Text
forceDisplayEvaluation = (>>= evaluate)

-- |Turns Block to displayable JSON. Executes displayText action and applies all modifiers.
blockToJson :: Block -> IO Text
blockToJson b = do
  t <- forceDisplayEvaluation (getDisplayedText b) `catch` handleBlockException
  return $ surroundBrackets . fixQuotes . apply $ t where
    surroundBrackets t = T.concat ["{\"markup\":\"pango\", \"full_text\":\"", t, "\"}"]
    fixQuotes :: Text -> Text
    fixQuotes = T.reverse . pack . fix "" . T.unpack where
      fix :: String -> String -> String
      fix acc s = case s of
        [] -> acc
        '"':t -> fix ('\"':'\\':acc) t -- because reverse!
        '\\':t -> fix acc t
        h:t -> fix (h:acc) t

    apply :: Text -> Text
    apply = foldl (flip (.)) id mods
    mods :: [Text -> Text]
    mods = [ applyPrefix
           , applySuffix
           , applyUnderline
           , applyBgColor
           , applyColor
           ]
    applyPrefix = (prefix b `T.append`)
    applySuffix = (`T.append` suffix b)
    applyUnderline = case underline b of
      None -> id
      u -> spanSurround "underline" (pack . show $ u)
    applyBgColor = case bgColor b of
      Nothing -> id
      Just c -> spanSurround "bgcolor" (pack . show $ c)
    applyColor = spanSurround "color" (pack . show $ color b)
