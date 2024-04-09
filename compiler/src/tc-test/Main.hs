{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Either (isLeft, isRight)
import Data.Text
import ParserTypes
import System.Exit (exitFailure, exitSuccess)
import Frontend.Tc.Tc
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Text where
    arbitrary = pack <$> listOf (elements "abcdefghijklmnopqrstuvwxyz")

instance Arbitrary AddOp where
    arbitrary = elements [Plus NoInfo, Minus NoInfo]

instance Arbitrary MulOp where
    arbitrary = elements (fmap ($ NoInfo) [Times, Div, Mod])

instance Arbitrary RelOp where
    arbitrary = elements (fmap ($ NoInfo) [LTH, LE, GTH, GE, EQU, NE])

genIntGood :: Gen Expr
genIntGood =
    frequency
        [ (55, ELitInt NoInfo <$> arbitrary)
        , (15, Neg NoInfo <$> genIntGood)
        , (15, EMul NoInfo <$> genIntGood <*> arbitrary <*> genIntGood)
        , (15, EAdd NoInfo <$> genIntGood <*> arbitrary <*> genIntGood)
        ]

genDoubleGood :: Gen Expr
genDoubleGood =
    frequency
        [ (55, ELitDouble NoInfo <$> arbitrary)
        , (15, EAdd NoInfo <$> genIntGood <*> arbitrary <*> genDoubleGood)
        , (15, EAdd NoInfo <$> genDoubleGood <*> arbitrary <*> genIntGood)
        , (15, EAdd NoInfo <$> genDoubleGood <*> arbitrary <*> genDoubleGood)
        , (15, EMul NoInfo <$> genIntGood <*> arbitrary <*> genDoubleGood)
        , (15, EMul NoInfo <$> genDoubleGood <*> arbitrary <*> genIntGood)
        , (15, EMul NoInfo <$> genDoubleGood <*> arbitrary <*> genDoubleGood)
        ]

genBoolGood :: Gen Expr
genBoolGood =
    frequency
        [ (20, return $ ELitTrue NoInfo)
        , (20, return $ ELitFalse NoInfo)
        , (10, EAnd NoInfo <$> genBoolGood <*> genBoolGood)
        , (10, EOr NoInfo <$> genBoolGood <*> genBoolGood)
        , (10, ERel NoInfo <$> genDoubleGood <*> arbitrary <*> genDoubleGood)
        , (10, ERel NoInfo <$> genIntGood <*> arbitrary <*> genIntGood)
        , (10, ERel NoInfo <$> genStringGood <*> arbitrary <*> genStringGood)
        , (10, Not NoInfo <$> genBoolGood)
        ]

genStringGood :: Gen Expr
genStringGood = EString NoInfo <$> arbitrary

newtype GoodExpr = Good {runGood :: Expr}
    deriving (Show)

newtype BadExpr = Bad {runBad :: Expr}
    deriving (Show)

instance Arbitrary GoodExpr where
    arbitrary =
        Good
            <$> oneof
                [ genBoolGood
                , genStringGood
                , genDoubleGood
                , genIntGood
                ]

genExprBad :: Gen Expr
genExprBad =
    oneof
        [ EAdd NoInfo
            <$> oneof [genBoolGood, genStringGood, genExprBad, genIntGood, genDoubleGood]
            <*> arbitrary
            <*> oneof [genBoolGood, genStringGood, genExprBad]
        , EAdd NoInfo
            <$> oneof [genBoolGood, genStringGood, genExprBad]
            <*> arbitrary
            <*> oneof [genBoolGood, genStringGood, genExprBad, genIntGood, genDoubleGood]
        , EMul NoInfo
            <$> oneof [genBoolGood, genStringGood, genExprBad, genIntGood, genDoubleGood]
            <*> arbitrary
            <*> oneof [genBoolGood, genStringGood, genExprBad]
        , EMul NoInfo
            <$> oneof [genBoolGood, genStringGood, genExprBad]
            <*> arbitrary
            <*> oneof [genBoolGood, genStringGood, genExprBad, genIntGood, genDoubleGood]
        , EOr NoInfo
            <$> oneof [genStringGood, genIntGood, genDoubleGood, genExprBad, genBoolGood]
            <*> oneof [genStringGood, genIntGood, genDoubleGood, genExprBad]
        , EOr NoInfo
            <$> oneof [genStringGood, genIntGood, genDoubleGood, genExprBad]
            <*> oneof [genStringGood, genIntGood, genDoubleGood, genExprBad, genBoolGood]
        , EAnd NoInfo
            <$> oneof [genStringGood, genIntGood, genDoubleGood, genExprBad, genBoolGood]
            <*> oneof [genStringGood, genIntGood, genDoubleGood, genExprBad]
        , EAnd NoInfo
            <$> oneof [genStringGood, genIntGood, genDoubleGood, genExprBad]
            <*> oneof [genStringGood, genIntGood, genDoubleGood, genExprBad, genBoolGood]
        , Neg NoInfo <$> oneof [genStringGood, genExprBad, genBoolGood]
        , Not NoInfo <$> oneof [genStringGood, genExprBad, genIntGood, genDoubleGood]
        , do
            let list = [oneof [genDoubleGood, genIntGood], genStringGood, genExprBad, genBoolGood]
            let len = Prelude.length list
            n <- elements [0 .. len - 1]
            m <- elements (Prelude.filter (/= n) [0 .. len - 1])
            op <- arbitrary
            left <- list !! n
            right <- list !! m
            return $ ERel NoInfo left op right
        ]

instance Arbitrary BadExpr where
    arbitrary = Bad <$> genExprBad

prop_checkExpr :: GoodExpr -> Bool
prop_checkExpr (Good e) = isRight $ run $ infExpr e

prop_failExpr :: BadExpr -> Bool
prop_failExpr (Bad e) = isLeft $ run $ infExpr e

return []

runTests = $quickCheckAll

main :: IO ()
main = do
    b <- runTests
    if b then exitSuccess else exitFailure
