{-# LANGUAGE OverloadedStrings #-}
module Example where
import Parser.ProgramParser (program)
import Prelude hiding (readFile)
import Data.Text.IO (readFile)

example1 :: IO ()
example1 = do
    let file = "inputs/prg.bl"
    inp <- readFile file
    print $ program file inp
