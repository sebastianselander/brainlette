{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module ParserTypes where

import Data.Text (Text)
import Text.Parsec (Parsec)
import GHC.Generics

type Parser a = Parsec Text () a

data SynInfo
    = SynInfo
        { sourceLine :: !Int
        , sourceColumn :: !Int
        , sourceName :: !Text
        , sourceCode :: !Text
        }
    | NoInfo
    deriving (Show, Generic)

instance Eq SynInfo where
    _ == _ = True

instance Ord SynInfo where
    compare _ _ = EQ

type Expr = Expr' SynInfo
type AddOp = AddOp' SynInfo
type Arg = Arg' SynInfo
type Item = Item' SynInfo
type MulOp = MulOp' SynInfo
type Prog = Prog' SynInfo
type RelOp = RelOp' SynInfo
type Stmt = Stmt' SynInfo
type TopDef = TopDef' SynInfo
type Type = Type' SynInfo
type Id = Id' SynInfo


data Prog' a = Program a [TopDef' a]
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data TopDef' a = FnDef a (Type' a) (Id' a) [Arg' a] [Stmt' a]
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Arg' a = Argument a (Type' a) (Id' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Item' a = NoInit a (Id' a) | Init a (Id' a) (Expr' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Stmt' a
    = Empty a
    | BStmt a [Stmt' a]
    | Decl a (Type' a) [Item' a]
    | Ass a (Id' a) (Expr' a)
    | Incr a (Id' a)
    | Decr a (Id' a)
    | Ret a (Expr' a)
    | VRet a
    | Cond a (Expr' a) (Stmt' a)
    | CondElse a (Expr' a) (Stmt' a) (Stmt' a)
    | While a (Expr' a) (Stmt' a)
    | Break a
    | SExp a (Expr' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Type' a
    = TVar a (Id' a)
    | Fun a (Type' a) [Type' a]
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Expr' a
    = EVar a (Id' a)
    | ELitInt a Integer
    | ELitDouble a Double
    | ELitTrue a
    | ELitFalse a
    | EApp a (Id' a) [Expr' a]
    | EString a Text
    | Neg a (Expr' a)
    | Not a (Expr' a)
    | EMul a (Expr' a) (MulOp' a) (Expr' a)
    | EAdd a (Expr' a) (AddOp' a) (Expr' a)
    | ERel a (Expr' a) (RelOp' a) (Expr' a)
    | EAnd a (Expr' a) (Expr' a)
    | EOr a (Expr' a) (Expr' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data AddOp' a
    = Plus a
    | Minus a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data MulOp' a
    = Times a
    | Div a
    | Mod a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data RelOp' a
    = LTH a
    | LE a
    | GTH a
    | GE a
    | EQU a
    | NE a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Id' a = Id a Text
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

pattern Int :: Type
pattern Int <- TVar _ (Id _ "int")
  where Int = TVar NoInfo (Id NoInfo "int")

pattern Double :: Type
pattern Double <- TVar _ (Id _ "double")
  where Double = TVar NoInfo(Id NoInfo "double")

pattern String :: Type
pattern String <- TVar _ (Id _ "string")
  where String = TVar NoInfo (Id NoInfo "string")

pattern Boolean :: Type
pattern Boolean <- TVar _ (Id _ "boolean")
  where Boolean = TVar NoInfo (Id NoInfo "boolean")

pattern Void :: Type
pattern Void <- TVar _ (Id _ "void")
  where Void = TVar NoInfo (Id NoInfo "void")
