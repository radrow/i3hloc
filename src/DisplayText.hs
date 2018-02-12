{-# LANGUAGE FlexibleInstances #-}

module DisplayText where

import Data.Text
import System.Exit

-- |Text to be displayed
-- |@text returns IO action with given text
-- |@postLen\Modifier describes function that modifies text after
-- |length calculation is done
data DisplayText = DisplayText
  { text :: Text
  , postLenModifier :: Text -> Text
  }

newDisplayText t = DisplayText t id

-- |Allows modules to return some different types instead of IO Text
class Display a where
  display :: a -> DisplayText

instance Display Text where
  display = newDisplayText

instance Display [Char] where
  display = newDisplayText . pack

instance Display Bool where
  display = newDisplayText . pack . show

instance Display Int where
  display = newDisplayText . pack . show

instance Display Integer where
  display = newDisplayText . pack . show

instance Display Double where
  display = newDisplayText . pack . show

instance Display Char where
  display = newDisplayText . pack . (:[])

instance Display ExitCode where
  display = newDisplayText . pack . show . readCode where
    readCode ExitSuccess = 0
    readCode (ExitFailure n) = n
