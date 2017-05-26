{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE ExistentialQuantification#-}

module Block where

import Control.Exception

import Colors
import Pango
import qualified Data.Text as T
import Data.Text(Text, pack, unpack)

data Block = Block
  { color :: Color
  , bgColor :: Maybe Color
  , fullText :: IO Text
  , prefix :: Text
  , suffix :: Text
  , underline :: UnderlineMode
  }

staticText :: Text -> IO Text
staticText = return


newBlock = Block { color = Colors.white
                 , bgColor = Nothing
                 , prefix = ""
                 , suffix = ""
                 , underline = None
                 }

handleBlockException :: SomeException -> IO Text
handleBlockException _ = return errMsg where
  errMsg = spanSurround "color" "red" "(BLOCK ERROR)"

forceBlockEvaluation :: IO Text -> IO Text
forceBlockEvaluation = (>>= evaluate)

blockToJson :: Block -> IO Text
blockToJson b = do
  t <- forceBlockEvaluation (fullText b) `catch` handleBlockException
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
    applyPrefix =  (prefix b `T.append`)
    applySuffix = (`T.append` suffix b)
    applyUnderline = case underline b of
      None -> id
      u -> spanSurround "underline" (pack . show $ u)
    applyBgColor = case bgColor b of
      Nothing -> id
      Just c -> spanSurround "bgcolor" (pack . show $ c)
    applyColor = spanSurround "color" (pack . show $ color b)

