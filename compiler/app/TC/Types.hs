{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Tc.Types where

import Ast
import Data.Text (Text)
import Text.Parsec (Parsec)

data Tc

type Parser a = Parsec Text () a

data InfoTc = InfoTc
    { sourceLine :: !Int
    , sourceColumn :: !Int
    , sourceName :: !Text
    , typ :: !Type
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

type instance XAddOp Tc = Void

type instance XArg Tc = Info

type instance XArgument Tc = Info

type instance XAss Tc = Info

type instance XBStmt Tc = Void

type instance XCond Tc = Void

type instance XCondElse Tc = Void

type instance XDecl Tc = Void

type instance XDecr Tc = Void

type instance XDiv Tc = Info

type instance XEAdd Tc = Info

type instance XEAnd Tc = Info

type instance XEApp Tc = Info

type instance XELitDoub Tc = Void

type instance XELitFalse Tc = Void

type instance XELitInt Tc = Void

type instance XELitTrue Tc = Void

type instance XEMul Tc = Void

type instance XEOr Tc = Void

type instance XEQU Tc = Void

type instance XERel Tc = Void

type instance XEString Tc = Void

type instance XEVar Tc = Void

type instance XEmpty Tc = Void

type instance XExpr Tc = Void

type instance XFnDef Tc = Void

type instance XFun Tc = Void

type instance XGE Tc = Void

type instance XGTH Tc = Void

type instance XIncr Tc = Void

type instance XInit Tc = Void

type instance XItem Tc = Void

type instance XLE Tc = Void

type instance XLTH Tc = Void

type instance XMinus Tc = Void

type instance XMod Tc = Void

type instance XMulOp Tc = Void

type instance XNE Tc = Void

type instance XNeg Tc = Void

type instance XNoInit Tc = Void

type instance XNot Tc = Void

type instance XPlus Tc = Void

type instance XProg Tc = Void

type instance XProgram Tc = Void

type instance XRelOp Tc = Void

type instance XRet Tc = Void

type instance XSExp Tc = Void

type instance XStmt Tc = Void

type instance XTVar Tc = Void

type instance XTimes Tc = Void

type instance XTopDef Tc = Void

type instance XType Tc = Void

type instance XVRet Tc = Void

type instance XWhile Tc = Void

type instance XId Tc = Void

pattern IdTc a <- Id _ a
    where
        IdTc a = Id undefined a

pattern EAddTc l op r <- EAdd _ l op r
    where
        EAddTc ty (Info l c s _) l op r = EAdd (InfoTc l c s ty) l op r


pattern ProgramTc a <- Program _ a
  where ProgramTc a = ProgramTc undefined a


pattern FnDefTc t i a s <- FnDef _ t i a s
  where FnDefTc t i a s = FnDefTc undefined t i a s 


pattern ArgumentTc t a <- Argument _ t a
  where ArgumentTc t a = Argument undefined t a

pattern NoInitTc a <- NoInit _ a
  where NoInitTc a = NoInit undefined a

pattern EmptyTc a <- Empty _ a
    where EmptyTc a <- Empty undefined a
    
pattern BStmtTc s <- BStmt _ s
    where BStmtTc a <- Stmt _ a

pattern DeclTc t i = Decl _ t i
    where DeclTc t i <- Decl undefined
    | Ass (XAss a) (Id a) (Expr a)
    | Incr (XIncr a) (Id a)
    | Decr (XDecr a) (Id a)
    | Ret (XRet a) (Expr a)
    | VRet (XVRet a)
    | Cond (XCond a) (Expr a) (Stmt a)
    | CondElse (XCondElse a) (Expr a) (Stmt a) (Stmt a)
    | While (XWhile a) (Expr a) (Stmt a)
    | SExp (XSExp a) (Expr a)

pattern TVarTc a <- TVar _ 
  where TVarTc a -> TVar undefined a

pattern FunTc ty tys <- Fun _ ty ts
  where FunTc ty tys = Fun 
e
-- data Expr a
--     = EVar (XEVar a) (Id a)
--     | ELitInt (XELitInt a) Integer
--     | ELitDouble (XELitDoub a) Double
--     | ELitTrue (XELitTrue a)
--     | ELitFalse (XELitFalse a)
--     | EApp (XEApp a) (Id a) [Expr a]
--     | EString (XEString a) Text
--     | Neg (XNeg a) (Expr a)
--     | Not (XNot a) (Expr a)
--     | EMul (XEMul a) (Expr a) (MulOp a) (Expr a)
--     | EAdd (XEAdd a) (Expr a) (AddOp a) (Expr a)
--     | ERel (XERel a) (Expr a) (RelOp a) (Expr a)
--     | EAnd (XEAnd a) (Expr a) (Expr a)
--     | EOr (XEOr a) (Expr a) (Expr a)
pattern PlusTc <- Plus _
  where PlusTc = Plus undefined

pattern MinusTc <- Minus _
  where MinusTc = Minus undefined

pattern TimesTc <- Times _
  where TimesTc = Times undefined

pattern DivTc <- Div _
  where DivTc = Div undefined

pattern ModTc <- Mod _
  where ModTc = Mod undefined

pattern LTHTc <- LTH _
  where LTHTc = LTH undefined

pattern LETc <- LE _
  where LETc = LE undefined

pattern GTHTc <- GTH _
  where GTHTc = GTH undefined

pattern GETc <- GE _
  where GETc =  GE undefined

pattern EQUTc <- EQU _
  where EQUTc = EQU undefined

pattern NETc <- NE _
  where NETc = NE undefined