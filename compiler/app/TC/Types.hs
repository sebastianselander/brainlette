{-# LANGUAGE TypeFamilies #-}

module TC.Types where

import Ast
import Data.Text (Text)
import ParserTypes (Info (..))

data Tc

data InfoTc = InfoTc
    { sourceLine :: !Int
    , sourceColumn :: !Int
    , sourceName :: !Text
    , sourceCode :: !Text
    , typ :: !TypeTc
    }
    deriving (Show)

type ExprTc = Expr Tc

type AddOpTc = AddOp Tc

type ArgTc = Arg Tc

type ItemTc = Item Tc

type MulOpTc = MulOp Tc

type ProgTc = Prog Tc

type RelOpTc = RelOp Tc

type StmtTc = Stmt Tc

type TopDefTc = TopDef Tc

type TypeTc = Type Tc

type IdTc = Id Tc

type instance XAddOp Tc     = Info
type instance XArg Tc       = InfoTc
type instance XArgument Tc  = Info
type instance XAss Tc       = InfoTc
type instance XBStmt Tc     = Info
type instance XCond Tc      = Info
type instance XCondElse Tc  = Info
type instance XDecl Tc      = Info
type instance XDecr Tc      = Info
type instance XDiv Tc       = Info
type instance XEAdd Tc      = InfoTc
type instance XEAnd Tc      = Info
type instance XEApp Tc      = InfoTc
type instance XELitDoub Tc  = Info
type instance XELitFalse Tc = Info
type instance XELitInt Tc   = Info
type instance XELitTrue Tc  = Info
type instance XEMul Tc      = InfoTc
type instance XEOr Tc       = Info
type instance XEQU Tc       = Info
type instance XERel Tc      = Info
type instance XEString Tc   = Info
type instance XEVar Tc      = InfoTc
type instance XEmpty Tc     = Info
type instance XExpr Tc      = Info
type instance XFnDef Tc     = Info
type instance XFun Tc       = Info
type instance XGE Tc        = Info
type instance XGTH Tc       = Info
type instance XId Tc        = InfoTc
type instance XIncr Tc      = Info
type instance XInit Tc      = Info
type instance XItem Tc      = Info
type instance XLE Tc        = Info
type instance XLTH Tc       = Info
type instance XMinus Tc     = Info
type instance XMod Tc       = Info
type instance XMulOp Tc     = Info
type instance XNE Tc        = Info
type instance XNeg Tc       = Info
type instance XNoInit Tc    = Info
type instance XNot Tc       = Info
type instance XPlus Tc      = Info
type instance XProg Tc      = Info
type instance XProgram Tc   = Info
type instance XRelOp Tc     = Info
type instance XRet Tc       = Info
type instance XSExp Tc      = Info
type instance XStmt Tc      = Info
type instance XTVar Tc      = Info
type instance XTimes Tc     = Info
type instance XTopDef Tc    = Info
type instance XType Tc      = Info
type instance XVRet Tc      = Info
type instance XWhile Tc     = Info
