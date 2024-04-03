{-# LANGUAGE PatternSynonyms #-}

module Utils where

import Data.Text (Text, pack)

{-# WARNING TODO "TODO" #-}
pattern TODO :: a
pattern TODO <- _
  where
    TODO = error "TODO: Not yet implemented"

-- | Map with flipped arguments
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Show but text :)
thow :: (Show a) => a -> Text
thow = pack . show