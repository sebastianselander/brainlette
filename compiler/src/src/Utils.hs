{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Utils where

import Data.Text (Text, intercalate, pack, replicate)
import GHC.Stack (HasCallStack)
import Prelude hiding (replicate)

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

flat3 :: (a, (b, c)) -> (a, b, c)
flat3 (a, (b, c)) = (a, b, c)

class Pretty a where
  {-# MINIMAL pretty #-}
  pretty :: Int -> a -> Text

  parenthesis :: Int -> a -> Text
  parenthesis n a = "(" <> pretty n a <> ")"

  indent :: Int -> a -> Text
  indent n a = replicate (n * 4) " " <> pretty n a

  commaSeparated :: Int -> [a] -> Text
  commaSeparated n = intercalate ", " . map (pretty n)

  semi :: Int -> a -> Text
  semi n a = pretty n a <> ";"

instance Pretty Text where
  pretty _ t = t