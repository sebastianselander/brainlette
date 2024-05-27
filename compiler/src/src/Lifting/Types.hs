{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lifting.Types where

import Data.Generics (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String (IsString)
import Data.String.Interpolate (i)
import Data.Text (Text, intercalate, pack)
import Utils

newtype Prog = Program [TopDef]
    deriving (Eq, Ord, Show, Read, Data)

instance Pretty Prog where
    pretty :: Int -> Prog -> Text
    pretty _ (Program td) = intercalate "\n\n" $ map (pretty 0) td

data TopDef
    = FnDef Toplevel Type Id [Arg] [Stmt]
    | StructDef Id [Arg]
    | TypeDef Type Id
    deriving (Eq, Ord, Show, Read, Data)

instance Pretty TopDef where
    pretty :: Int -> TopDef -> Text
    pretty n = \case
        FnDef tl t id args st ->
            let st' = intercalate "\n" $ map (semi (n + 1)) st
                gap = pack $ replicate (n * 4) ' '
                tl' = if tl then "" :: Text else "lifted-"
             in [i|\ESC[91m#{tl'}function\ESC[0m #{pretty 1 id} (#{commaSeparated 0 args}) -> #{pretty 0 t} {\n#{st'}\n#{gap}}|]
        StructDef id ts ->
            let ts' = commaSeparated 0 ts
             in [i|\ESC[91mstruct\ESC[0m #{pretty 0 id} = {#{ts'}}|]
        TypeDef t id -> [i|typedef #{pretty 0 id} = #{pretty 0 t}|]

data Arg = Argument Type Id
    deriving (Eq, Ord, Show, Read, Data)

instance Pretty Arg where
    pretty :: Int -> Arg -> Text
    pretty _ (Argument t id) = [i|#{pretty 0 t} #{pretty 0 id}|]

data LValue
    = LVar Id
    | LDeref Expr Id
    | LIndex Expr Expr
    | LStructIndex Expr Id
    deriving (Eq, Ord, Show, Read, Data)

instance Pretty LValue where
    pretty :: Int -> LValue -> Text
    pretty _ = \case
        LVar id -> pretty 0 id
        LDeref e int -> [i|#{pretty 0 e}->#{int}|]
        LIndex b index -> [i|#{pretty 0 b}[#{pretty 0 index}]|]
        LStructIndex e int -> [i|#{pretty 0 e}.#{int}|]

data Stmt
    = BStmt [Stmt]
    | -- | The expression always represents the size of the array
      ArrayNew Type Id (NonEmpty Expr)
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
    | ForI Stmt Expr Stmt Stmt
    | SExp Expr
    | Break
    | ExtractFree Type Int Id
    | LoadSelf (Type, Id) (Type, Id)
    deriving (Eq, Ord, Show, Read, Data)

instance Pretty Stmt where
    pretty :: Int -> Stmt -> Text
    pretty n =
        indent n . \case
            BStmt bs -> "{\n" <> intercalate "\n" (map (semi (n + 1)) bs) <> "\n" <> indent n ("}" :: Text)
            Decl t i -> pretty 0 t <> " " <> intercalate ", " (map (pretty 0) i)
            Ass ty l e -> "@" <> pretty 0 ty <> " " <> pretty 0 l <> " = " <> pretty 0 e
            Ret e -> "return " <> pretty 0 e
            VRet -> "return-void"
            CondElse e s1 s2 ->
                let els = "else" :: Text
                 in [i|if (#{pretty 0 e})\n#{pretty n s1}\n#{indent n els}\n#{pretty n s2}|]
            While e s ->
                [i|while (#{pretty 0 e})
#{pretty (n + 1) s}|]
            SExp e -> pretty 0 e
            Break -> "break"
            Incr t v -> [i|(#{pretty 0 t} #{pretty 0 v}) = #{pretty 0 v} + 1|]
            Decr t v -> [i|(#{pretty 0 t} #{pretty 0 v}) = #{pretty 0 v} - 1|]
            Cond e s -> [i|if (#{pretty 0 e})\n#{pretty n s}|]
            ForEach a e s ->
                [i|foreach (#{pretty 0 a} : #{pretty 0 e})
#{pretty (n + 1) s}|]
            ArrayNew t id exprs ->
                [i|#{pretty 0 t} #{pretty 0 id} = [#{intercalate ", " $ map (pretty 0) (NonEmpty.toList exprs)}]|]
            ExtractFree t int name -> [i|#{pretty 0 t} #{pretty 0 name} = #{pretty 0 (Id "$captures$")}[#{int}]|]
            LoadSelf (_, fn) (_, name) -> "self-load {" <> pretty n fn <> "} to {" <> pretty n name <> "}"
            ForI s1 e s2 body -> [i|for (#{pretty 0 s1} #{pretty 0 e}; #{pretty n s2})
#{pretty (n + 1) body}|]

data Item = NoInit Id | Init Id Expr
    deriving (Eq, Ord, Show, Read, Data)

instance Pretty Item where
    pretty _ = \case
        NoInit id -> pretty 0 id
        Init id e -> [i|#{pretty 0 id} = #{pretty 0 e}|]

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
    | Closure Type
    deriving (Eq, Ord, Show, Read, Data)

instance Pretty Type where
    pretty :: Int -> Type -> Text
    pretty _ = \case
        TVar (Id i) -> i
        String -> "String"
        Double -> "Double"
        Void -> "void"
        Boolean -> "Bool"
        Int -> "Int"
        Fun rt ts -> [i|(#{commaSeparated 0 ts}) -> #{pretty 0 rt}|]
        Pointer t -> pretty 0 t <> "*"
        Array t -> pretty 0 t <> "[]"
        Closure ty -> "Closure<" <> pretty 0 ty <> ">"

type Expr = (Type, Expr')

instance Pretty Expr where
    pretty :: Int -> Expr -> Text
    -- pretty n (t, e) = indent n ([i|#{pretty 0 e}|] :: Text)
    -- Disable pretty printing of types
    pretty n (t, e) = indent n ([i|(#{pretty 0 e} : #{pretty 0 t})|] :: Text)

data Expr'
    = EVar Id
    | ELiftedVar Id
    | ELit Lit
    | EApp Expr [Expr]
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
    | ClosureLit Expr [Expr]
    deriving (Eq, Ord, Show, Read, Data)

instance Pretty Expr' where
    pretty :: Int -> Expr' -> Text
    pretty n =
        indent n . \case
            EVar i -> pretty 0 i
            ELit l -> pretty 0 l
            EApp id exp -> [i|#{pretty 0 id} (#{commaSeparated 0 exp})|]
            Not e -> "!" <> pretty 0 e
            Neg e -> "-" <> pretty 0 e
            EMul e1 op e2 -> [i|#{pretty 0 e1} #{pretty 0 op} #{pretty 0 e2}|]
            EAdd e1 op e2 -> [i|#{pretty 0 e1} #{pretty 0 op} #{pretty 0 e2}|]
            ERel e1 op e2 -> [i|#{pretty 0 e1} #{pretty 0 op} #{pretty 0 e2}|]
            EAnd e1 e2 -> [i|#{pretty 0 e1} && #{pretty 0 e2}|]
            EOr e1 e2 -> [i|#{pretty 0 e1} || #{pretty 0 e2}|]
            Deref e id -> [i|#{pretty 0 e}->#{id}|]
            StructIndex e id -> [i|#{pretty 0 e}.#{id}|]
            ArrayLit l -> [i|[#{intercalate ", " $ map (pretty 0) l}]|]
            StructAlloc -> "new"
            ArrayIndex b ind -> [i|#{pretty 0 b}[#{pretty 0 ind}]|]
            ELiftedVar id -> [i|lifted #{pretty 0 id}|]
            ClosureLit b e -> [i|Closure{#{pretty 0 b}, [#{intercalate ", " $ map (pretty 0) e }]}|]

data Lit
    = LitInt Integer
    | LitDouble Double
    | LitBool Bool
    | LitString Text
    | LitNull
    deriving (Eq, Ord, Show, Read, Data)

instance Pretty Lit where
    pretty :: Int -> Lit -> Text
    pretty _ = \case
        LitInt l -> thow l
        LitDouble l -> thow l
        LitBool l -> thow l
        LitString l -> thow l
        LitNull -> "null"

data AddOp = Plus | Minus
    deriving (Eq, Ord, Show, Read, Data)

instance Pretty AddOp where
    pretty :: Int -> AddOp -> Text
    pretty _ = \case
        Plus -> "+"
        Minus -> "-"

data MulOp = Times | Div | Mod
    deriving (Eq, Ord, Show, Read, Data)

instance Pretty MulOp where
    pretty :: Int -> MulOp -> Text
    pretty _ = \case
        Times -> "*"
        Div -> "/"
        Mod -> "%"

data RelOp = LTH | LE | GTH | GE | EQU | NE
    deriving (Eq, Ord, Show, Read, Data)

instance Pretty RelOp where
    pretty :: Int -> RelOp -> Text
    pretty _ = \case
        LTH -> "<"
        LE -> "<="
        GTH -> ">"
        GE -> ">="
        EQU -> "=="
        NE -> "!="

newtype Id = Id Text
    deriving (Eq, Ord, Show, Read, IsString, Data)

instance Pretty Id where
    pretty _ (Id id) = indent 0 ("\ESC[93m" <> id <> "\ESC[0m")
