{-# LANGUAGE FlexibleInstances #-}

module DisplayText where

import Data.Text
import System.Exit

-- |Text to be displayed
newtype DisplayText = DisplayText Text

-- |Allows modules to return some different types instead of IO Text
class Display a where
  display :: a -> DisplayText

instance Display Text where
  display = DisplayText

instance Display [Char] where
  display = DisplayText . pack

instance Display Bool where
  display = DisplayText . pack . show

instance Display Int where
  display = DisplayText . pack . show

instance Display Integer where
  display = DisplayText . pack . show

instance Display Double where
  display = DisplayText . pack . show

instance Display Char where
  display = DisplayText . pack . (:[])

instance Display ExitCode where
  display = DisplayText . pack . show . readCode where
    readCode ExitSuccess = 0
    readCode (ExitFailure n) = n
