{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Frontend.Tc.Types where

import Data.String (IsString)
import Data.Text (Text)

newtype Prog = Program [TopDef]
    deriving (Eq, Ord, Show, Read)

data TopDef = FnDef Type Id [Arg] [Stmt]
    deriving (Eq, Ord, Show, Read)

data Arg = Argument Type Id
    deriving (Eq, Ord, Show, Read)

data Stmt
    = BStmt [Stmt]
    | Decl Type [Item]
    | Ass Id Expr
    | Incr Type Id
    | Decr Type Id
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | SExp Expr
    | Break
    deriving (Eq, Ord, Show, Read)

data Item = NoInit Id | Init Id Expr
    deriving (Eq, Ord, Show, Read)

data Type
    = TVar Id
    | Fun Type [Type]
    deriving (Eq, Ord, Show, Read)

pattern Int :: Type
pattern Int <- TVar (Id "int")
  where Int = TVar (Id "int")

pattern Double :: Type
pattern Double <- TVar (Id "double")
  where Double = TVar (Id "double")

pattern String :: Type
pattern String <- TVar (Id "string")
  where String = TVar (Id "string")

pattern Boolean :: Type
pattern Boolean <- TVar (Id "boolean")
  where Boolean = TVar (Id "boolean")

pattern Void :: Type
pattern Void <- TVar (Id "void")
  where Void = TVar (Id "void")

type Expr = (Type, Expr')

data Expr'
    = EVar Id
    | ELit Lit
    | EApp Id [Expr]
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    deriving (Eq, Ord, Show, Read)

data Lit
    = LitInt Integer
    | LitDouble Double
    | LitBool Bool
    | LitString Text
    deriving (Eq, Ord, Show, Read)

data AddOp = Plus | Minus
    deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
    deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
    deriving (Eq, Ord, Show, Read)

newtype Id = Id Text
    deriving (Eq, Ord, Show, Read, IsString)
