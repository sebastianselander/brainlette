module Main where

import System.Environment
import System.Exit

import Brainlette.Par (pProg, myLexer)
import TC.Typechecker

main :: IO ()
main = do
    [file] <- getArgs
    text <- readFile file
    res <- case pProg (myLexer text) of
        Left err -> print err *> exitFailure
        Right res -> return res
    res <- case tc res of
        Left err -> print err *> exitFailure
        Right res -> return res
    print res
