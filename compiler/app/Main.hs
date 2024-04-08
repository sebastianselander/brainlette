{-# LANGUAGE OverloadedStrings #-}

module Main where

import BMM.TcToBmm (bmm)
import Braingen.Ir (braingen)
import BrainletteParser
import ParserTypes
import Control.Monad (unless)
import Data.Text (pack, unpack)
import Frontend.BranchReturns (check)
import Frontend.Tc.Tc (tc)
import System.Directory (doesFileExist)
import System.Environment
import System.Exit
import Utils (ePrint, ePutStrLn)
import Frontend.Renamer (rename)
import Data.Functor (void)

main :: IO ()
main = do
    args <- getArgs
    file <- case args of
        [] -> ePutStrLn "brainlette: no input file" >> exitFailure
        (file : _) -> do
            b <- doesFileExist file
            unless b $ ePutStrLn "brainlette: file does not exist" >> exitFailure
            return file
    text <- readFile file

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
