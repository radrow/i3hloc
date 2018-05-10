module Block( makeBlock
            , blockToJson
            , Block( underline
                   , color
                   , bgColor
                   , prefix
                   , suffix
                   , displayText
                   , minWidth
                   )
            , errorBlock
            ) where

import Control.Exception

import Colors
import Pango hiding (Error)
import DisplayText

import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans(lift)
import Data.Maybe
import qualified Data.Text as T
import Data.Text(Text, pack)
import System.IO(stderr, hPutStrLn)

-- |Block is responsible for displaying text from selected module with
-- |certain modifications applied
data Block = Block
  { color :: Maybe Color
  , bgColor :: Maybe Color
  , displayText :: IO DisplayText
  , prefix :: Text
  , suffix :: Text
  , underline :: UnderlineMode
  , minWidth :: Maybe Integer
  }

-- |Default Block creator with undefined displayText. It may not be exported.
newBlock :: Block
newBlock = Block { displayText = undefined
                 , color = Nothing
                 , bgColor = Nothing
                 , prefix = ""
                 , suffix = ""
                 , underline = None
                 , minWidth = Nothing
                 }

-- |Creates default block using given DisplayText
makeBlock :: IO DisplayText -> Block
makeBlock d = newBlock{displayText = d}

-- |Default block that displays error message
errorBlock :: String -> Block
errorBlock errorText = newBlock
  { displayText = return $ display errorText
  , color = Just red
  , prefix = "["
  , suffix = "]"
  }

-- |Used when Block throws an exception. Prints to stderr message of exception
-- |and returns error message
handleBlockException :: SomeException -> IO DisplayText
handleBlockException e = do
  hPutStrLn stderr ("\n" ++ displayException e ++ "\n")
  return $ display $ spanSurround "color" "red" "[BLOCK ERROR (e)]"

-- |Forces evaluation of block's displayText, to face all exceptions
forceDisplayEvaluation :: IO DisplayText -> IO DisplayText
forceDisplayEvaluation = (>>= evaluate)

-- |Turns Block to displayable JSON. Executes displayText action and applies all modifiers.
blockToJson :: Block -> IO Text
blockToJson b = execWriterT $ do
  tell "{"

  let param a b = T.concat ["\"", a, "\":", b]
      param' a b = param a b `T.append` ","

      quote t = T.concat ["\"", t, "\""]

  tell $ param' "markup" (quote "pango")
  tell $ param' "align" (quote "center")
  case color b of
    Nothing -> return ()
    Just (Color t) -> tell $ param' "color" (quote t)
  case bgColor b of
    Nothing -> return ()
    Just (Color t) -> tell $ param' "background" (quote t)
  case minWidth b of
    Nothing -> return ()
    Just i -> tell $ param' "min_width" (pack $ show i)

  let fixQuotes :: Text -> Text
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
      mods = [ id
             , applyPrefix
             , applySuffix
             , applyUnderline
             ]
      applyPrefix = (prefix b `T.append`)
      applySuffix = (`T.append` suffix b)
      applyUnderline = case underline b of
        None -> id
        u -> spanSurround "underline" (pack . show $ u)

  (DisplayText t) <- lift $ forceDisplayEvaluation (displayText b) `catch` handleBlockException

  tell $ param "full_text" $ (quote . fixQuotes . apply) t
  tell "}"


