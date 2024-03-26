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
type Type = Type' Position

type AddOp = AddOp' Position
type MulOp = MulOp' Position
type RelOp = RelOp' Position

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

data Type' a = Int a | Double a | Bool a | String a | Void a | Fun a (Type' a) [Type' a]
    deriving (Eq, Ord, Show, Read)

data Expr' a
    = EVar a Type Ident
    | ELit a Type Lit
    | EApp a Type Ident [Expr' a]
    | Neg a Type (Expr' a)
    | Not a (Expr' a)
    | EMul a Type (Expr' a) (MulOp' a) (Expr' a)
    | EAdd a Type (Expr' a) (AddOp' a) (Expr' a)
    | ERel a (Expr' a) (RelOp' a) (Expr' a)
    | EAnd a (Expr' a) (Expr' a)
    | EOr a (Expr' a) (Expr' a)
    deriving (Eq, Ord, Show, Read,Functor)

data Lit
    = LitInt Integer
    | LitDouble Double
    | LitBool Bool
    | LitString String
    deriving (Eq, Ord, Show, Read)

data AddOp' a = Plus a | Minus a
    deriving (Eq, Ord, Show, Read, Functor)

data MulOp' a = Times a | Div a | Mod a
    deriving (Eq, Ord, Show, Read, Functor)

data RelOp' a = LTH a | LE a | GTH a | GE a | EQU a | NE a
    deriving (Eq, Ord, Show, Read, Functor)

newtype Ident = Ident String
    deriving (Eq, Ord, Show, Read, IsString)
