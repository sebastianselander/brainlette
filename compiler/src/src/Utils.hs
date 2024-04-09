{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Utils where

import Data.Text (Text, pack)
import Data.Text.IO (hPutStrLn)
import GHC.Stack (HasCallStack)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)

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

-- | Print string to stdErr
ePutStrLn :: Text -> IO ()
#if DEBUG
ePutStrLn = hPutStrLn stderr
#else
ePutStrLn _ = pure ()
#endif

-- | Print error and exit
errorExit :: forall a. Text -> IO a
#if DEBUG
errorExit err = hPutStrLn stderr err *> exitFailure
#else
errorExit _ = hPutStrLn stderr "ERROR" *> exitFailure
#endif

-- | Print OK
ok :: IO ()
#if DEBUG
ok = pure ()
#else
ok = hPutStrLn stderr "OK"
#endif

ePrint :: (Show a) => a -> IO ()
ePrint = ePutStrLn . thow
