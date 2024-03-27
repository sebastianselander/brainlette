{-# LANGUAGE OverloadedStrings #-}
module Example where
import Parser.ProgramParser (program)
import Prelude hiding (readFile)
import Data.Text.IO (readFile)

example1 :: IO ()
example1 = do
    let file = "inputs/example1.bl"
    inp <- readFile file
    print $ program file inp

example2 :: IO ()
example2 = do
    let file = "inputs/example2.bl"
    inp <- readFile file
    print $ program file inp
