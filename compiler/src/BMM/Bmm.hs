{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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


pattern Int :: Type
pattern Int <- TVar (Id "int")
  where Int = TVar (Id "int")

pattern Boolean :: Type
pattern Boolean <- TVar (Id "boolean")
  where Boolean = TVar (Id "boolean")

pattern Void :: Type
pattern Void <- TVar (Id "void")
  where Void = TVar (Id "void")

pattern Double :: Type
pattern Double <- TVar (Id "double")
  where Double = TVar (Id "double")
