{-# LANGUAGE TypeFamilies #-}

module ParserTypes where

import Ast
import Data.Text (Text)
import Text.Parsec (Parsec)

type Parser a = Parsec Text () a

data InfoSyn
    = InfoSyn
        { sourceLine :: !Int
        , sourceColumn :: !Int
        , sourceName :: !Text
        , sourceCode :: !Text
        }
    | NoInfoSyn
    deriving (Show)

type ExprSyn = ExprSyn' InfoSyn
type AddOpSyn = AddOpSyn' InfoSyn
type ArgSyn = ArgSyn' InfoSyn
type ItemSyn = ItemSyn' InfoSyn
type MulOpSyn = MulOpSyn' InfoSyn
type ProgSyn = ProgSyn' InfoSyn
type RelOpSyn = RelOpSyn' InfoSyn
type StmtSyn = StmtSyn' InfoSyn
type TopDefSyn = TopDefSyn' InfoSyn
type TypeSyn = TypeSyn' InfoSyn
type IdSyn = IdSyn' InfoSyn
