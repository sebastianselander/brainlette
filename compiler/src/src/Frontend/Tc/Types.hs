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
    | Int
    | Double
    | String
    | Void
    | Boolean
    deriving (Eq, Ord, Show, Read)

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
