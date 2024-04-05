{-# LANGUAGE PatternSynonyms #-}

module Utils where

import Data.Text (Text, pack)
import Data.Text.IO (hPutStrLn)
import System.IO (hPrint, stderr)

{-# WARNING TODO "TODO" #-}
pattern TODO :: a
pattern TODO <- _
  where
    TODO = error "TODO: Not yet implemented"

-- | Map with flipped arguments
for :: [a] -> (a -> b) -> [b]
for = flip map

concatFor :: [a] -> (a -> [b]) -> [b]
concatFor = flip concatMap

-- | Show but text :)
thow :: (Show a) => a -> Text
thow = pack . show

-- | Print string to stdErr
ePutStrLn :: Text -> IO ()
ePutStrLn = hPutStrLn stderr

ePrint :: (Show a) => a -> IO ()
ePrint = hPrint stderr