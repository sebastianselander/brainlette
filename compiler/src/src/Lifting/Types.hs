module Lifting.Types where

import Data.String (IsString)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

newtype Prog = Program [TopDef]
    deriving (Eq, Ord, Show, Read)

data TopDef
    = FnDef Type Id [Arg] [Stmt]
    | StructDef Id [Arg]
    | TypeDef Type Id
    deriving (Eq, Ord, Show, Read)

data Arg = Argument Type Id
    deriving (Eq, Ord, Show, Read)

data LValue = LVar Id | LDeref Expr Id | LIndex Expr Expr | LStructIndex Expr Id
    deriving (Eq, Ord, Show, Read)

data Stmt
    = BStmt [Stmt]
    -- | The expression always represents the size of the array
    | ArrayNew Type Id (NonEmpty Expr)
    | StructNew Type Id Id
    | Decl Type [Item]
    | Ass Type LValue Expr
    | Incr Type Id
    | Decr Type Id
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | ForEach Arg Expr Stmt
    | SExp Expr
    | Break
    deriving (Eq, Ord, Show, Read)

data Item = NoInit Id | Init Id Expr
    deriving (Eq, Ord, Show, Read)

data Type
    = TVar Id
    | Fun Type [Type]
    | String
    | Int
    | Double
    | Boolean
    | Void
    | Pointer Type
    | Array Type
    deriving (Eq, Ord, Show, Read)

type Expr = (Type, Expr')

data Expr'
    = EVar Id
    | ELit Lit
    | EApp Id [Expr]
    | Neg Expr
    | Not Expr
    | Deref Expr Id
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    | ArrayIndex Expr Expr
    | StructAlloc 
    | StructIndex Expr Id
    | ArrayLit [Expr]
    deriving (Eq, Ord, Show, Read)

data Lit
    = LitInt Integer
    | LitDouble Double
    | LitBool Bool
    | LitString Text
    | LitNull
    deriving (Eq, Ord, Show, Read)

data AddOp = Plus | Minus
    deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
    deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
    deriving (Eq, Ord, Show, Read)

newtype Id = Id Text
    deriving (Eq, Ord, Show, Read, IsString)
