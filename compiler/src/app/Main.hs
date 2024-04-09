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
import Frontend.Renamer (rename)
import Data.String.Interpolate

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

    ePutStrLn "--- Parse output ---"
    res <- case program file (pack text) of
        Left err -> print err *> exitFailure
        Right res -> return res

    ePutStrLn (pretty 0 res)

    ePutStrLn "--- Renamer output ---"

    res <- case rename res of
        Left err -> ePutStrLn err *> exitFailure
        Right res -> return res

    ePutStrLn (pretty 0 res)

    res <- case check res of
        Left err -> ePutStrLn err *> exitFailure
        Right res -> return res

    ePutStrLn "\n--- Check output ---"
    ePutStrLn (pretty 0 res)

    res <- case tc res of
        Left err -> ePutStrLn err *> exitFailure
        Right res -> return res

    ePutStrLn "\n--- Typecheck output ---"
    ePrint res

    res <- case bmm res of
        Left err -> ePutStrLn err *> exitFailure
        Right res -> return res

    ePutStrLn "\n--- BMM output ---"
    ePrint res

    res <- case braingen res of
        Left err -> ePutStrLn err *> exitFailure
        Right res -> return res

    ePutStrLn "\n--- LLVM IR output ---"

    putStrLn $ unpack res

prelude :: Text
prelude = pack [i|
void printInt(int a) {}
|]
