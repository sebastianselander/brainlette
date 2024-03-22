module TTypes where

import Data.String (IsString)

newtype Prog = Program [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef = FnDef Type Ident [Arg] Blk
  deriving (Eq, Ord, Show, Read)

data Arg = Argument Type Ident
  deriving (Eq, Ord, Show, Read)

newtype Blk = Block [Stmt]
  deriving (Eq, Ord, Show, Read)

data Stmt
    = Empty
    | BStmt Blk
    | Decl Type [Item]
    | Ass Type Ident Expr
    | Incr Ident
    | Decr Ident
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | SExp Expr
  deriving (Eq, Ord, Show, Read)

data Item = NoInit Ident | Init Ident Expr
  deriving (Eq, Ord, Show, Read)

data Type = Int | Doub | Bool | Void | Fun Type [Type]
  deriving (Eq, Ord, Show, Read)

data Expr
    = EVar Type Ident
    | ELitInt Integer
    | ELitDoub Double
    | ELitTrue
    | ELitFalse
    | EApp Type Ident [Expr]
    | EString String
    | Neg Type Expr
    | Not Expr
    | EMul Type Expr MulOp Expr
    | EAdd Type Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
  deriving (Eq, Ord, Show, Read)

data AddOp = Plus | Minus
  deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
  deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (Eq, Ord, Show, Read)

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read, IsString)

