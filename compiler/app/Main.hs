module Main where

import Brainlette.Par (pProg, myLexer)
import System.Environment
import System.Exit

main :: IO ()
main = do
    [file] <- getArgs
    text <- readFile file
    res <- case pProg (myLexer text) of
        Left err -> print err >> exitFailure
        Right res -> return res
    print res
