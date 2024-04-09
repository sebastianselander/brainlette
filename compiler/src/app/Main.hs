{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BMM.TcToBmm (bmm)
import Braingen.Ir (braingen)
import Control.Monad (unless)
import Data.Text (pack, unpack, Text)
import Frontend.BranchReturns (check)
import Frontend.Parser.BrainletteParser
import Frontend.Parser.ParserTypes
import Frontend.Renamer (rename)
import Frontend.Tc.Tc (tc)
import System.Directory (doesFileExist)
import System.Environment
import System.Exit
import Utils (ePrint, ePutStrLn)
import Data.String.Interpolate
import Utils (thow)

main :: IO ()
main = do
    args <- getArgs
    (file, text) <- case args of
        [] -> ("stdin",) <$> getLine
        (file : _) -> do
            b <- doesFileExist file
            unless b $ ePutStrLn "brainlette: file does not exist" >> exitFailure
            text <- readFile file
            return (file, text)

    let text' = text <> prelude

#if DEBUG
    ePutStrLn "--- Parse output ---"
#endif
    res <- case program file (pack text') of
        Left err -> errorExit (thow err)
        Right res -> return res
#if DEBUG
    ePutStrLn (pretty 0 res)
#endif

#if DEBUG
    ePutStrLn "--- Renamer output ---"
#endif
    res <- case rename res of
        Left err -> errorExit err
        Right res -> return res

#if DEBUG
    ePutStrLn (pretty 0 res)
#endif
    res <- case check res of
        Left err -> errorExit err
        Right res -> return res

#if DEBUG
    ePutStrLn "\n--- Check output ---"
#endif    ePutStrLn (pretty 0 res)

    res <- case tc res of
        Left err -> errorExit err
        Right res -> return res

#if DEBUG
    ePutStrLn "\n--- Typecheck output ---"
    ePrint res
#endif

    res <- case bmm res of
        Left err -> errorExit err
        Right res -> return res

#if DEBUG
    ePutStrLn "\n--- BMM output ---"
    ePrint res
#endif

    res <- case braingen res of
        Left err -> errorExit err
        Right res -> return res

#if DEBUG
    ePutStrLn "\n--- LLVM IR output ---"
    putStrLn $ unpack res
#endif
    ePutStrLn "OK"
    return ()

errorExit :: forall a . Text -> IO a
#if DEBUG
errorExit err = ePutStrLn err *> exitFailure
#else
errorExit _ = ePutStrLn "ERROR" *> exitFailure
#endif


prelude :: String
prelude = [i|
    void printInt(int a) {}
    void printDouble(double a) {}
    void printString(string a) {}
|]
