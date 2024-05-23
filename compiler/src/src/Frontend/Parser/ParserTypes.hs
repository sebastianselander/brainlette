{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Parser.ParserTypes where

import Data.Text (Text, concat, cons, intercalate, pack, replicate, unlines, unwords)
import GHC.Generics
import Generics.SYB (Data, Typeable)
import Text.Parsec (Parsec)
import Utils (Pretty (..))
import Prelude hiding (concat, replicate, unlines, unwords)

type Parser a = Parsec Text () a

data SynInfo
    = SynInfo
        { sourceLine :: !Int
        , sourceColumn :: !Int
        , sourceName :: !Text
        , sourceCode :: !Text
        }
    | NoInfo
    deriving (Show, Generic, Data, Typeable)

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
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

data TopDef' a
    = FnDef a (Type' a) (Id' a) [Arg' a] [Stmt' a]
    | StructDef a (Id' a) [Arg' a]
    | TypeDef a (Id' a) (Id' a)
    | Use a (Id' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

data Arg' a = Argument a (Type' a) (Id' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

data Item' a = NoInit a (Id' a) | Init a (Id' a) (Expr' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

data Stmt' a
    = Empty a
    | BStmt a [Stmt' a]
    | Decl a (Type' a) [Item' a]
    | Ass a (Expr' a) (Expr' a)
    | Incr a (Id' a)
    | Decr a (Id' a)
    | Ret a (Expr' a)
    | VRet a
    | Cond a (Expr' a) (Stmt' a)
    | CondElse a (Expr' a) (Stmt' a) (Stmt' a)
    | While a (Expr' a) (Stmt' a)
    | ForEach a (Arg' a) (Expr' a) (Stmt' a)
    | Break a
    | SExp a (Expr' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

data Type' a
    = TVar a (Id' a)
    | Fun a (Type' a) [Type' a]
    | String a
    | Int a
    | Double a
    | Void a
    | Boolean a
    | Pointer a (Type' a)
    | Array a (Type' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

data Expr' a
    = EVar a (Id' a)
    | ELitInt a Integer
    | ELitDouble a Double
    | ELitTrue a
    | ELitFalse a
    | ELitNull a (Maybe (Type' a))
    | EString a Text
    | ENew a (Type' a) [Expr' a]
    | EDeref a (Expr' a) (Id' a)
    | EStructIndex a (Expr' a) (Id' a)
    | EIndex a (Expr' a) (Expr' a)
    | EApp a (Id' a) [Expr' a]
    | Neg a (Expr' a)
    | Not a (Expr' a)
    | EMul a (Expr' a) (MulOp' a) (Expr' a)
    | EAdd a (Expr' a) (AddOp' a) (Expr' a)
    | ERel a (Expr' a) (RelOp' a) (Expr' a)
    | EAnd a (Expr' a) (Expr' a)
    | EOr a (Expr' a) (Expr' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

data AddOp' a
    = Plus a
    | Minus a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

data MulOp' a
    = Times a
    | Div a
    | Mod a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

data RelOp' a
    = LTH a
    | LE a
    | GTH a
    | GE a
    | EQU a
    | NE a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

data Id' a = Id a (Maybe Text) Text
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

pattern IdD :: a -> Text -> Id' a
pattern IdD info text <- Id info Nothing text
    where
        IdD info text = Id info Nothing text

instance Pretty RelOp where
    pretty _ = \case
        LE _ -> "<="
        LTH _ -> "<"
        GTH _ -> ">"
        GE _ -> ">="
        EQU _ -> "=="
        NE _ -> "!="

instance Pretty Id where
    pretty _ (Id _ (Just n) a) = n <> "::" <> a
    pretty _ (Id _ Nothing a) = a

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
        ELitNull _ ty -> maybe "" (pretty n) ty <> "null"
        EString _ txt -> pretty n txt
        EDeref _ left right -> parenthesis n $ concat [pretty n left, "->", pretty n right]
        EApp _ i exprs -> pretty n i <> parenthesis n (intercalate ", " $ map (pretty n) exprs)
        Neg _ expr -> '-' `cons` pretty n expr
        Not _ expr -> '!' `cons` pretty n expr
        EMul _ l op r -> parenthesis n $ unwords [pretty n l, pretty n op, pretty n r]
        EAdd _ l op r -> parenthesis n $ unwords [pretty n l, pretty n op, pretty n r]
        ERel _ l op r -> parenthesis n $ unwords [pretty n l, pretty n op, pretty n r]
        EAnd _ l r -> parenthesis n $ unwords [pretty n l, pretty n r]
        EOr _ l r -> parenthesis n $ unwords [pretty n l, pretty n r]
        ENew _ name sizes ->
            pretty n $
                unwords ["new", pretty n name] <> case map (\e -> "[" <> pretty n e <> "]") sizes of
                    [] -> ""
                    xs -> concat xs
        EIndex _ e1 e2 -> pretty n $ unwords [pretty n e1, "[" <> pretty n e2 <> "]"]
        EStructIndex _ expr field -> pretty n $ concat [pretty n expr, ".", pretty n field]

instance Pretty Type where
    pretty n (String _) = replicate n " " <> "string"
    pretty n (Void _) = replicate n " " <> "void"
    pretty n (Int _) = replicate n " " <> "int"
    pretty n (Double _) = replicate n " " <> "double"
    pretty n (Boolean _) = replicate n " " <> "boolean"
    pretty n (TVar _ ident) = pretty n ident
    pretty n (Fun _ ret args) = pretty n ret <> parenthesis n (intercalate ", " $ map (pretty n) args)
    pretty n (Pointer _ ty) = pretty n ty <> "*"
    pretty n (Array _ ty) = pretty n ty <> "[]"

instance Pretty Stmt where
    pretty n = \case
        Empty _ -> ""
        BStmt _ stmts -> "{\n" <> unlines (map (indent (n + 1)) stmts) <> "\n" <> indent @Text n "}"
        Decl _ ty items -> semi n $ unwords [pretty n ty, intercalate ", " $ map (pretty n) items]
        Ass _ lv expr -> semi n $ unwords [pretty n lv, "=", pretty n expr]
        Incr _ ident -> pretty n ident <> "++"
        Decr _ ident -> pretty n ident <> "--"
        Ret _ expr -> semi n $ unwords ["return", pretty n expr]
        VRet _ -> "return;"
        Cond _ expr stmt -> unlines ["if " <> parenthesis n expr, pretty n stmt]
        CondElse _ expr stmt1 stmt2 -> unlines ["if " <> parenthesis n expr, pretty n stmt1, "else", pretty n stmt2]
        While _ expr stmt -> unlines ["while " <> parenthesis n expr, pretty n stmt]
        ForEach _ arg expr stmt -> unlines ["for" <> parenthesis n (pretty n arg <> " : " <> pretty n expr), pretty n stmt]
        Break _ -> "break;"
        SExp _ expr -> semi n expr

instance Pretty Item where
    pretty n = \case
        Init _ ident expr -> unwords [pretty n ident, "=", pretty n expr]
        NoInit _ ident -> pretty n ident

instance Pretty Arg where
    pretty n (Argument _ ty ident) = unwords [pretty n ty, pretty n ident]

instance Pretty TopDef where
    pretty n (StructDef _ ident fields) =
        concat
            [ "struct "
            , pretty n ident
            , " {\n"
            , unlines (map ((<> ";") . indent n) fields)
            , "};"
            ]
    pretty n (FnDef _ ty ident args stmts) =
        unwords
            [ pretty n ty
            , pretty n ident
            , parenthesis n $ commaSeparated n args
            , "{\n" <> unlines (map (indent n) stmts) <> "}"
            ]
    pretty n (TypeDef _ t1 t2) =
        concat ["typedef struct ", pretty n t1, " *", pretty n t2, ";"]
    pretty _ (Use _ i) =
        "use " <> pretty 0 i <> ";"

instance Pretty Prog where
    pretty n (Program _ xs) = go xs
      where
        go [] = ""
        go (x : xs) = pretty (n + 1) x <> "\n\n" <> go xs
