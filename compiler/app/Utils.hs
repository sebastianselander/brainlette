{-# LANGUAGE PatternSynonyms #-}

module Utils where

{-# WARNING TODO "TODO" #-}
pattern TODO :: a
pattern TODO <- _
  where
    TODO = error "TODO: Not yet implemented"

-- | Map with flipped arguments
for :: [a] -> (a -> b) -> [b]
for = flip map