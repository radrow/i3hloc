module Block where

import Control.Exception

import Colors
import Pango

data Block = Block
  { color :: Color
  , bgColor :: Maybe Color
  , fullText :: IO String
  , prefix :: String
  , suffix :: String
  , underline :: UnderlineMode
  }

staticText :: String -> IO String
staticText = return

newBlock = Block { color = Colors.white
                , bgColor = Nothing
                , prefix = ""
                , suffix = ""
                , underline = None
                }

handleBlockException :: SomeException -> IO String
handleBlockException _ = return errMsg where
  errMsg = spanSurround "color" "red" "(BLOCK ERROR)"

forceBlockEvaluation :: IO String -> IO String
forceBlockEvaluation bmonad = do
  b <- bmonad
  evaluate b

blockToJson :: Block -> IO String
blockToJson b = do
  t <- forceBlockEvaluation (fullText b) `catch` handleBlockException
  return $ surroundBrackets . fixQuotes . apply $ t where
    surroundBrackets t = "{\"markup\":\"pango\", \"full_text\":\"" ++ t ++ "\"}"
    fixQuotes :: String -> String
    fixQuotes = reverse . fix [] where
      fix :: String -> String -> String
      fix acc s = case s of
        [] -> acc
        '"':t -> fix ('\"':'\\':acc) t -- because reverse!
        h:t -> fix (h:acc) t

    apply :: String -> String
    apply = foldl (flip (.)) id mods
    mods :: [String -> String]
    mods = [ applyPrefix
           , applySuffix
           , applyUnderline
           , applyBgColor
           , applyColor
           ]
    applyPrefix =  (prefix b ++)
    applySuffix = (++ suffix b)
    applyUnderline = case underline b of
      None -> id
      u -> spanSurround "underline" (show u)
    applyBgColor = case bgColor b of
      Nothing -> id
      Just c -> spanSurround "bgcolor" (show c)
    applyColor = spanSurround "color" (show (color b))

