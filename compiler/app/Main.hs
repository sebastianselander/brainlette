module Main where

import System.Environment
import System.Exit

import Brainlette.Par (pProg, myLexer)
import TC.Typechecker
import System.IO (hPutStrLn, stderr)
import System.Directory (doesFileExist)
import Control.Monad (unless)

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
    hPutStrLn stderr "Parsing"
    res <- case pProg (myLexer text) of
        Left err -> print err *> exitFailure
        Right res -> return res
    hPutStrLn stderr "Type checking"
    res <- case tc res of
        Left err -> hPutStrLn stderr err *> exitFailure
        Right res -> return res
    print res
