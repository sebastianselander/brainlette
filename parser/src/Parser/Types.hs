{-# LANGUAGE TypeFamilies #-}

module Parser.Types where

import Ast.Types
import Data.Text (Text)
import Text.Parsec (Parsec)

data Syn

type Parser a = Parsec Text () a

data Info
    = Info
        { sourceLine :: !Int
        , sourceColumn :: !Int
        , sourceName :: !Text
        , sourceCode :: !Text
        }
    | NoInfo
    deriving (Show)

type ExprSyn = Expr Syn
type AddOpSyn = AddOp Syn
type ArgSyn = Arg Syn
type ItemSyn = Item Syn
type MulOpSyn = MulOp Syn
type ProgSyn = Prog Syn
type RelOpSyn = RelOp Syn
type StmtSyn = Stmt Syn
type TopDefSyn = TopDef Syn
type TypeSyn = Type Syn
type IdSyn = Id Syn

type instance XAddOp Syn = Info
type instance XArg Syn = Info
type instance XArgument Syn = Info
type instance XAss Syn = Info
type instance XBStmt Syn = Info
type instance XCond Syn = Info
type instance XCondElse Syn = Info
type instance XDecl Syn = Info
type instance XDecr Syn = Info
type instance XDiv Syn = Info
type instance XEAdd Syn = Info
type instance XEAnd Syn = Info
type instance XEApp Syn = Info
type instance XELitDoub Syn = Info
type instance XELitFalse Syn = Info
type instance XELitInt Syn = Info
type instance XELitTrue Syn = Info
type instance XEMul Syn = Info
type instance XEOr Syn = Info
type instance XEQU Syn = Info
type instance XERel Syn = Info
type instance XEString Syn = Info
type instance XEVar Syn = Info
type instance XEmpty Syn = Info
type instance XExpr Syn = Info
type instance XFnDef Syn = Info
type instance XFun Syn = Info
type instance XGE Syn = Info
type instance XGTH Syn = Info
type instance XIncr Syn = Info
type instance XInit Syn = Info
type instance XItem Syn = Info
type instance XLE Syn = Info
type instance XLTH Syn = Info
type instance XMinus Syn = Info
type instance XMod Syn = Info
type instance XMulOp Syn = Info
type instance XNE Syn = Info
type instance XNeg Syn = Info
type instance XNoInit Syn = Info
type instance XNot Syn = Info
type instance XPlus Syn = Info
type instance XProg Syn = Info
type instance XProgram Syn = Info
type instance XRelOp Syn = Info
type instance XRet Syn = Info
type instance XSExp Syn = Info
type instance XStmt Syn = Info
type instance XTVar Syn = Info
type instance XTimes Syn = Info
type instance XTopDef Syn = Info
type instance XType Syn = Info
type instance XVRet Syn = Info
type instance XWhile Syn = Info
type instance XId Syn = Info
