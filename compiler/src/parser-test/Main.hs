{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (isLeft, isRight)
import Data.Text
import Frontend.Parser.ParserTypes
import Frontend.Parser.TypeParser
import Frontend.Parser.TypeParser qualified as Parser
import Frontend.Tc.Tc
import System.Exit (exitFailure, exitSuccess)
import Test.Hspec
import Text.Parsec hiding (parserFail)

parserSuccess :: (Show a) => Parser a -> Text -> Expectation
parserSuccess p input = parse p "" input `shouldSatisfy` isRight

parserFail :: (Show a) => Parser a -> Text -> Expectation
parserFail p input = parse p "" input `shouldSatisfy` isLeft

main :: IO ()
main = hspec do
    describe "Parser.custom" do
        it "it should succed on lower case names" do
            parserSuccess custom "foo"
            parserSuccess custom "bar"
        it "should fail on trying to parse reserved words" do
            parserFail custom "int"
            parserFail custom "string"
            parserFail custom "void"
            parserFail custom "double"
            parserFail custom "boolean"
