module Config( Config
             ) where

import Block

-- |Configuration is actually list of blocks. No more features yet
type Config = [Block]
