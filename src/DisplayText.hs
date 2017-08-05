{-# LANGUAGE FlexibleInstances #-}

module DisplayText where

import Data.Text
import System.Exit

-- |IO Text to be displayed for Blocks
newtype DisplayText = DisplayText (IO Text)

-- |Allows modules to return some different types instead of IO Text
class Display a where
  display :: IO a -> DisplayText

instance Display Text where
  display = DisplayText

instance Display [Char] where
  display s = DisplayText (pack <$> s)

instance Display Bool where
  display = DisplayText . (pack . show <$>)

instance Display Int where
  display = DisplayText . (pack . show <$>)

instance Display Integer where
  display = DisplayText . (pack . show <$>)

instance Display Double where
  display = DisplayText . (pack . show <$>)

instance Display Char where
  display = DisplayText . (pack . (:[]) <$>)

instance Display ExitCode where
  display = DisplayText . (pack . show . readCode <$>) where
    readCode ExitSuccess = 0
    readCode (ExitFailure n) = n
