{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module ParserTypes where

import Data.Text (Text, pack, intercalate, cons, unwords, unlines, replicate)
import Text.Parsec (Parsec)
import GHC.Generics
import Prelude hiding (unwords, unlines, replicate)

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
  where Double = TVar NoInfo (Id NoInfo "double")

pattern String :: Type
pattern String <- TVar _ (Id _ "string")
  where String = TVar NoInfo (Id NoInfo "string")

pattern Boolean :: Type
pattern Boolean <- TVar _ (Id _ "boolean")
  where Boolean = TVar NoInfo (Id NoInfo "boolean")

pattern Void :: Type
pattern Void <- TVar _ (Id _ "void")
  where Void = TVar NoInfo (Id NoInfo "void")

class Pretty a where
    {-# MINIMAL pretty #-}
    pretty :: Int -> a -> Text

    parenthesis :: Int -> a -> Text
    parenthesis n a = "(" <> pretty n a <> ")"

    indent :: Int -> a -> Text
    indent n a = replicate (n * 4) " " <> pretty n a

    commaSeparated :: Int -> [a] -> Text
    commaSeparated n = intercalate ", " . map (pretty n)

    semi :: Int -> a -> Text
    semi n a = pretty n a <> ";"

instance Pretty Text where
    pretty n t = t

instance Pretty RelOp where
    pretty _ = \case
      LTH _ -> "<"
      LE _ -> "<="
      GTH _ -> ">"
      GE _ -> ">="
      EQU _ -> "=="
      NE _ -> "!="

instance Pretty Id where
    pretty _ (Id _ a) = a

instance Pretty MulOp where
    pretty _ (Times _) = "*"
    pretty _ (Div _) = "/"
    pretty _ (Mod _) = "%"

instance Pretty AddOp where
    pretty _ (Plus _) = "+"
    pretty _ (Minus _) = "-"

instance Pretty Expr where
    pretty n = \case
      EVar _ ident -> pretty n ident
      ELitInt _ n -> pack $ show n
      ELitDouble _ n -> pack $ show n
      ELitTrue _ -> "true"
      ELitFalse _ -> "false"
      EApp _ i exprs -> pretty n i <> parenthesis n (intercalate ", " $ map (pretty n) exprs)
      EString _ txt -> pretty n txt
      Neg _ expr -> '-' `cons` pretty n expr
      Not _ expr -> '!' `cons` pretty n expr
      EMul _ l op r -> parenthesis n $ unwords [pretty n l, pretty n op, pretty n r]
      EAdd _ l op r -> parenthesis n $ unwords [pretty n l, pretty n op, pretty n r]
      ERel _ l op r -> parenthesis n $ unwords [pretty n l, pretty n op, pretty n r]
      EAnd _ l r -> parenthesis n $ unwords [pretty n l, pretty n r]
      EOr _ l r -> parenthesis n $ unwords [pretty n l, pretty n r]

instance Pretty Type where
    pretty n (TVar _ ident) = pretty n ident
    pretty n (Fun _ ret args) = pretty n ret <> parenthesis n (intercalate ", " $ map (pretty n) args)

instance Pretty Stmt where
    pretty n = \case
        Empty _ -> ""
        BStmt _ stmts -> "{\n" <> unlines (map (indent (n + 1)) stmts) <> "\n" <> indent @Text n "}"
        Decl _ ty items -> semi n $ unwords [pretty n ty, intercalate ", " $ map (pretty n) items]
        Ass _ ident expr -> semi n $ unwords [pretty n ident, "=", pretty n expr]
        Incr _ ident -> pretty n ident <> "++"
        Decr _ ident -> pretty n ident <> "--"
        Ret _ expr -> semi n $ unwords ["return", pretty n expr]
        VRet _ -> "return;"
        Cond _ expr stmt -> unlines ["if " <> parenthesis n expr, pretty n stmt]
        CondElse _ expr stmt1 stmt2 -> unlines ["if " <> parenthesis n expr, pretty n stmt1, "else", pretty n stmt2]
        While _ expr stmt -> unlines ["while " <> parenthesis n expr, pretty n stmt]
        Break _ -> "break;"
        SExp _ expr -> semi n expr

instance Pretty Item where
    pretty n = \case
        Init _ ident expr -> unwords [pretty n ident, "=", pretty n expr]
        NoInit _ ident -> pretty n ident

instance Pretty Arg where
    pretty n (Argument _ ty ident) = unwords [pretty n ty, pretty n ident]

instance Pretty TopDef where
    pretty n (FnDef _ ty ident args stmts) = unwords [pretty n ty, pretty n ident, commaSeparated n args, "{\n" <> unlines (map (indent n) stmts) <> "\n}"]

instance Pretty Prog where
    pretty n (Program _ xs) = go xs
      where
        go [] = ""
        go (x:xs) = pretty (n + 1) x <> "\n\n" <> go xs
