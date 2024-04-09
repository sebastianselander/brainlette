{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Utils where

import Data.Text (Text, pack)
import GHC.Stack (HasCallStack)

{-# WARNING TODO "TODO" #-}
pattern TODO :: a
pattern TODO <- _
  where
    TODO = error "TODO: Not yet implemented"

{-# WARNING todo "todo" #-}
todo :: (HasCallStack) => a
todo = error "TODO: Not yet implemented"

-- | map with flipped arguments
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | concatMap with flipped arguments
concatFor :: [a] -> (a -> [b]) -> [b]
concatFor = flip concatMap

-- | Show but text :)
thow :: (Show a) => a -> Text
thow = pack . show
