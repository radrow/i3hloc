{-# LANGUAGE FlexibleInstances #-}

module DisplayText where

import Data.Text

newtype DisplayText = DisplayText (IO Text)

class Display a where
  display :: IO a -> DisplayText

instance Display Text where
  display = DisplayText

instance Display [Char] where
  display s = DisplayText (pack <$> s)

instance Display Bool where
  display = DisplayText . (pack . show <$>)

instance Display Integer where
  display = DisplayText . (pack . show <$>)

instance Display Double where
  display = DisplayText . (pack . show <$>)

instance Display Char where
  display = DisplayText . (pack . (:[]) <$>)
