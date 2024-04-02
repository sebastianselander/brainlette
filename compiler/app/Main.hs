{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.Exit

import System.Directory (doesFileExist)
import Control.Monad (unless)
import BrainletteParser
import Data.Text (pack)
import TypeChecker.Tc (tc)
import Data.Text.IO (hPutStrLn)
import System.IO (stderr)

main :: IO ()
main = do
    args <- getArgs
    file <- case args of
        [] -> hPutStrLn stderr "brainlette: no input file" >> exitFailure
        (file:_) -> do
            b <- doesFileExist file
            unless b $ hPutStrLn stderr "brainlette: file does not exist" >> exitFailure
            return file
    text <- readFile file
    res <- case program file (pack text) of
        Left err -> print err *> exitFailure
        Right res -> return res
    res <- case tc res of
        Left err -> hPutStrLn stderr err *> exitFailure
        Right res -> return res
    print res
