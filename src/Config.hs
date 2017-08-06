module Config( Config(blocks, printRepeats, updatePeriod)
             , newConfig
             ) where

import Block

-- |Main configuration type
data Config =
  Config { blocks :: [Block]
         , printRepeats :: Int
         , updatePeriod :: Integer
         }

newConfig :: Config
newConfig = Config
  { blocks = []
  , printRepeats = 20 -- TODO: fix defaults
  , updatePeriod = 1000000
  }
