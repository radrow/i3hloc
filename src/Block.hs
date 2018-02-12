module Block( makeBlock
            , blockToJson
            , Block( underline
                   , color
                   , bgColor
                   , prefix
                   , suffix
                   , displayText
                   , maxChars
                   , minChars
                   )
            , errorBlock
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
  , displayText :: IO DisplayText
  , prefix :: Text
  , suffix :: Text
  , underline :: UnderlineMode
  , maxChars :: Maybe Integer
  , minChars :: Maybe Integer
  }

-- |Default Block creator with undefined displayText. It may not be exported.
newBlock :: Block
newBlock = Block { displayText = undefined
                 , color = Colors.white
                 , bgColor = Nothing
                 , prefix = ""
                 , suffix = ""
                 , underline = None
                 , maxChars = Nothing
                 , minChars = Nothing
                 }

-- |Creates default block using given DisplayText
makeBlock :: IO DisplayText -> Block
makeBlock d = newBlock{displayText = d}

-- |Default block that displays error message
errorBlock :: String -> Block
errorBlock errorText = newBlock
  { displayText = return $ display errorText
  , color = red
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
blockToJson b = do
  (DisplayText t modifier) <- forceDisplayEvaluation (displayText b) `catch` handleBlockException

  let surroundBrackets t = T.concat ["{\"markup\":\"pango\", \"full_text\":\"", t, "\"}"]
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
      mods = [ id
             , applyMin
             , applyMax
             , modifier
             , applyPrefix
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
      applyMax = case maxChars b of
        Nothing -> id
        Just x -> T.take (fromInteger x)
      applyMin = case minChars b of
        Nothing -> id
        Just x -> let fillTo :: Int -> Text -> Text
                      fillTo n t = T.concat
                        [ pack . take ((n - T.length t) `div` 2) $ repeat ' '
                        , t
                        , pack . take (uncurry (+) ((n - T.length t) `divMod` 2)) $ repeat ' '
                        ]
                  in fillTo (fromInteger x)


  return $ surroundBrackets . fixQuotes . apply $ t where

