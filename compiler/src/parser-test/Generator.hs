{-# LANGUAGE OverloadedStrings #-}

module Generator where

import Data.Coerce (coerce)
import Data.Text
import Test.QuickCheck

newtype PType = PType Text
    deriving (Show, Eq, Ord)

mkPointer :: PType -> PType
mkPointer (PType t) = PType $ '*' `cons` t

mkArray (PType t) = PType $ t <> "[]"

instance Arbitrary PType where
    arbitrary = do
        let baseType =
                PType <$> elements @Text
                    [ "string"
                    , "void"
                    , "boolean"
                    , "int"
                    , "double"
                    ]
            varTypes = PType <$> elements @Text ["foo", "bar", "baz"]
            pointerType = mkPointer <$> arbitrary
            arrType = mkArray <$> arbitrary
        frequency [(25, baseType), (25, varTypes), (25, arrType)]
