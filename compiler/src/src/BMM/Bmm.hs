module BMM.Bmm where

import Data.Text (Text)

newtype Prog = Program [TopDef] deriving (Show)

data TopDef
    = FnDef Type Id [Arg] [Stmt]
    | StringGlobal Text Text
    deriving (Show)

data Arg = Argument Type Id deriving (Show)

data Stmt
    = BStmt [Stmt]
    | Decl Type Id
    | Ass Id Expr
    | Ret (Maybe Expr)
    | CondElse Expr [Stmt] [Stmt]
    | Loop Expr [Stmt]
    | SExp Expr
    | Break
    deriving (Show)

data Type
    = TVar Id
    | String
    | Double
    | Void
    | Boolean
    | Int
    | Fun Type [Type]
    deriving (Show)

type Expr = (Type, Expr')

data Expr'
    = EVar Id -- implemented
    | EGlobalVar Id
    | ELit Lit -- implemented
    | EApp Id [Expr] -- implemented
    | Not Expr
    | EMul Expr MulOp Expr -- implemented
    | EAdd Expr AddOp Expr -- implemented
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    | Neg Expr
    | Cast Expr
    deriving (Show)

data Lit
    = LitInt Integer
    | LitDouble Double
    | LitBool Bool
    | LitString Text
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
newtype Id = Id Text 
    deriving (Show, Eq, Ord)
