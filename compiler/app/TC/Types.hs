module TC.Types where
import Data.String (IsString)

type Position = Maybe (Int, Int)

type Prog = Prog' Position
type TopDef = TopDef' Position
type Arg = Arg' Position
type Blk = Blk' Position
type Stmt = Stmt' Position
type Item = Item' Position
type Expr = Expr' Position

newtype Prog' a = Program [TopDef' a]
    deriving (Eq, Ord, Show, Read, Functor)

data TopDef' a = FnDef a Type Ident [Arg' a] (Blk' a)
    deriving (Eq, Ord, Show, Read, Functor)

data Arg' a = Argument a Type Ident
    deriving (Eq, Ord, Show, Read, Functor)

data Blk' a = Block a [Stmt' a]
    deriving (Eq, Ord, Show, Read, Functor)

data Stmt' a
    = Empty
    | BStmt a (Blk' a)
    | Decl a Type [Item' a]
    | Ass a Type Ident (Expr' a)
    | Incr a Ident
    | Decr a Ident
    | Ret a (Expr' a)
    | VRet a
    | Cond a (Expr' a) (Stmt' a)
    | CondElse a (Expr' a) (Stmt' a) (Stmt' a)
    | While a (Expr' a) (Stmt' a)
    | SExp a (Expr' a)
    deriving (Eq, Ord, Show, Read, Functor)

data Item' a = NoInit a Ident | Init a Ident (Expr' a)
    deriving (Eq, Ord, Show, Read,Functor)

data Type = Int | Double | Bool | String | Void | Fun Type [Type]
    deriving (Eq, Ord, Show, Read)

data Expr' a
    = EVar a Type Ident
    | ELit a Type Lit
    | EApp a Type Ident [Expr' a]
    | Neg a Type (Expr' a)
    | Not a (Expr' a)
    | EMul a Type (Expr' a) MulOp (Expr' a)
    | EAdd a Type (Expr' a) AddOp (Expr' a)
    | ERel a (Expr' a) RelOp (Expr' a)
    | EAnd a (Expr' a) (Expr' a)
    | EOr a (Expr' a) (Expr' a)
    deriving (Eq, Ord, Show, Read,Functor)

data Lit
    = LitInt Integer
    | LitDouble Double
    | LitBool Bool
    | LitString String
    deriving (Eq, Ord, Show, Read)

data AddOp = Plus | Minus
    deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
    deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
    deriving (Eq, Ord, Show, Read)

newtype Ident = Ident String
    deriving (Eq, Ord, Show, Read, IsString)
