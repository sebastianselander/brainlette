{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BMM.TcToBmm (bmm)
import Braingen.Ir (braingen)
import Control.Monad (unless)
import Data.Text (Text, unlines)
import Data.Text.IO (getContents, hPutStrLn, putStrLn, readFile, writeFile)
import Frontend.BranchReturns (branchCheck)
import Frontend.Error (report)
import Frontend.Parser.BrainletteParser
import Frontend.Renamer (rename)
import Frontend.Tc.Tc (tc)
import Frontend.Uniter (importFiles)
import Lifting.Lifter (lift)
import Runtime (runtime)
import System.Directory (doesFileExist)
import System.Environment
import System.Exit
import System.IO (stderr)
import Utils (Pretty (..), thow)
import Prelude hiding (getContents, putStrLn, readFile, unlines, writeFile)

main :: IO ()
main = do
    args <- getArgs
    (file, text) <- case args of
        [] -> ("stdin",) <$> getContents
        (file : _) -> do
            b <- doesFileExist file
            unless b $ ePutStrLn "brainlette: file does not exist" >> exitFailure
            text <- readFile file
            return (file, text)

    ePutStrLn "--- Parse output ---"
    res <- case program file text of
        Left err -> errorExit (thow err)
        Right res -> return res

    -- ePutStrLn "--- Files to import ---"
    res <- importFiles res
    res <- case res of
        Left err -> errorExit (report err)
        Right res -> return res
    ePutStrLn (pretty 0 res)

    ePutStrLn "--- Renamer output ---"
    (res, _) <- case rename res of
        Left err -> errorExit err
        Right res -> return res

    ePutStrLn (pretty 0 res)
    res <- case branchCheck res of
        Left err -> errorExit err
        Right res -> return res

    ePutStrLn "\n--- Branch check output ---"

    ePutStrLn (pretty 0 res)

    res <- case tc res of
        Left err -> errorExit err
        Right res -> return res

    ePutStrLn "\n--- Typecheck output ---"
    ePutStrLn (pretty 0 res)

    ePutStrLn "\n--- Lifter output ---"

    (res, lifteds) <- return (lift res)
    ePutStrLn (pretty 0 res)

    ePutStrLn "\n--- BMM output ---"

    (res, funs) <- return $ bmm res
    ePutStrLn (pretty 0 res)
    writeFile "output.bmm" (pretty 0 res)

    res <- return $ braingen res lifteds funs

    ePutStrLn "\n--- LLVM IR output ---"
    let res' = unlines [runtime, res]
    ePutStrLn res'

    writeFile "output.ll" res'
    putStrLn res'
    ok

-- | Print string to stdErr
ePutStrLn :: Text -> IO ()
#if DEBUG
ePutStrLn = hPutStrLn stderr
#else
ePutStrLn _ = pure ()
#endif

-- | Print error and exit
errorExit :: forall a. Text -> IO a
#ifdef DEBUG
errorExit err = hPutStrLn stderr err *> exitFailure
#else
errorExit _ = hPutStrLn stderr "ERROR" *> exitFailure
#endif

-- | Print OK
ok :: IO ()
#ifdef DEBUG
ok = pure ()
#else
ok = hPutStrLn stderr "OK"
#endif

ePrint :: (Show a) => a -> IO ()
ePrint = ePutStrLn . thow
