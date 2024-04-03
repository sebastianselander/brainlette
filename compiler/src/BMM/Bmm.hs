{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BMM.Bmm where

import Data.Text (Text)

newtype Prog = Program [TopDef] deriving (Show)

data TopDef = FnDef Type Id [Arg] [Stmt] deriving (Show)

data Arg = Argument Type Id deriving (Show)

data Stmt
    = BStmt [Stmt]
    | Decl Type Id
    | Ass Id Expr
    | Ret (Maybe Expr)
    | CondElse Expr Stmt Stmt
    | Loop Stmt
    | SExp Expr
    deriving (Show)

data Type
    = TVar Id
    | Fun Type [Type]
    deriving (Show)

data Expr
    = EVar Id
    | ELitInt Integer
    | ELitDouble Double
    | ELitTrue
    | ELitFalse
    | EApp Id [Expr]
    | EString Text
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    deriving (Show)

{- Additive Operator -}
data AddOp
    = Plus
    | Minus
    deriving (Show)

{- Multiplicative Operator -}
data MulOp
    = Times
    | Div
    | Mod
    deriving (Show)

{-  Relational Operator -}
data RelOp
    = LTH
    | LE
    | GTH
    | GE
    | EQU
    | NE
    deriving (Show)

-- Identifier
newtype Id = Id Text deriving (Show)
