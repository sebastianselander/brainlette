{-# LANGUAGE PatternSynonyms #-}

module Utils where

{-# WARNING TODO "TODO" #-}
pattern TODO :: a
pattern TODO <- _
  where
    TODO = error "TODO: Not yet implemented"

for :: [a] -> (a -> b) -> [b]
for = flip map