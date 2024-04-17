{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import BMM.TcToBmm (bmm)
import Braingen.Ir (braingen)
import Control.Monad (unless)
import Data.String.Interpolate
import Data.Text (Text, pack)
import Data.Text.IO (hPutStrLn, writeFile)
import Frontend.BranchReturns (check)
import Frontend.Parser.BrainletteParser
import Frontend.Parser.ParserTypes
import Frontend.Renamer (rename)
import Frontend.Tc.Tc (tc)
import System.Directory (doesFileExist)
import System.Environment
import System.Exit
import System.IO (stderr)
import Utils (thow)
import Prelude hiding (writeFile)

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

    let text' = text <> prelude

    ePutStrLn "--- Parse output ---"
    res <- case program file (pack text') of
        Left err -> errorExit (thow err)
        Right res -> return res
    ePutStrLn (pretty 0 res)

    ePutStrLn "--- Renamer output ---"
    res <- case rename res of
        Left err -> errorExit err
        Right res -> return res

    ePutStrLn (pretty 0 res)
    res <- case check res of
        Left err -> errorExit err
        Right res -> return res

    ePutStrLn "\n--- Check output ---"

    res <- case tc res of
        Left err -> errorExit err
        Right res -> return res

    ePutStrLn "\n--- Typecheck output ---"
    ePrint res

    res <- case bmm res of
        Left err -> errorExit err
        Right res -> return res

    ePutStrLn "\n--- BMM output ---"
    ePrint res

    res <- case braingen res of
        Left err -> errorExit err
        Right res -> return res

    ePutStrLn "\n--- LLVM IR output ---"
    ePutStrLn res
    writeFile "output.ll" res
    ok

prelude :: String
prelude =
    [i|
    void printInt(int a) {}
    void printDouble(double a) {}
    void printString(string a) {}
    double readDouble() { return 0.0;}
    int readInt() { return 0;}
|]

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
