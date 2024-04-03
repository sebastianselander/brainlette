{-# LANGUAGE OverloadedStrings #-}

module Main where

import BMM.TcToBmm (bmm)
import BrainletteParser
import BranchReturns (check)
import Control.Monad (unless)
import Data.Text (pack)
import Data.Text.IO (hPutStrLn)
import System.Directory (doesFileExist)
import System.Environment
import System.Exit
import System.IO (stderr)
import TypeChecker.Tc (tc)

main :: IO ()
main = do
    args <- getArgs
    file <- case args of
        [] -> hPutStrLn stderr "brainlette: no input file" >> exitFailure
        (file : _) -> do
            b <- doesFileExist file
            unless b $ hPutStrLn stderr "brainlette: file does not exist" >> exitFailure
            return file
    text <- readFile file
    res <- case program file (pack text) of
        Left err -> print err *> exitFailure
        Right res -> return res
    res <- case check res of
        Left err -> hPutStrLn stderr err *> exitFailure
        Right res -> return res
    res <- case tc res of
        Left err -> hPutStrLn stderr err *> exitFailure
        Right res -> return res
    res <- case bmm res of
        Left err -> hPutStrLn stderr err *> exitFailure
        Right res -> return res

    print res
