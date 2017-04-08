module Block where

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


blockToJson :: Block -> IO String
-- blockToJson b = (\ t -> "{\"markup\":\"pango\", \"full_text\": \""
--                        ++ colorString (color b) t ++ "\"}") <$> (fullText b)

blockToJson b = do
  t <- fullText b
  return $ surroundBrackets . apply $ t where
    surroundBrackets t = "{\"markup\":\"pango\", \"full_text\":\"" ++ t ++ "\"}"
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

