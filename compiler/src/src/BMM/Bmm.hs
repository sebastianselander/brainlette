{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BMM.Bmm where

import Data.Dynamic (Typeable)
import Data.String.Interpolate (i)
import Data.Text (Text, intercalate)
import Generics.SYB (Data)
import Utils (Pretty (..), Toplevel, thow)
import Prelude hiding (concat, concatMap, takeWhile)

newtype Prog = Program [TopDef] deriving (Show, Data, Typeable)

instance Pretty Prog where
    pretty :: Int -> Prog -> Text
    pretty _ (Program tds) = intercalate "\n" $ map (pretty 0) tds

data TopDef
    = FnDef Toplevel Type Id [Arg] [Stmt]
    | StructDef Id [Type]
    | StringGlobal Text Text
    deriving (Show, Data, Typeable)

instance Pretty TopDef where
    pretty :: Int -> TopDef -> Text
    pretty _ = \case
        FnDef _ t id args st ->
            let st' = intercalate "\n" $ map (semi 1) st
             in [i|\ESC[91mfunction\ESC[0m #{pretty 1 id} (#{commaSeparated 0 args}) -> #{pretty 0 t} {\n#{st'}\n}|]
        StructDef id ts ->
            let ts' = commaSeparated 0 ts
             in [i|\ESC[91mstruct\ESC[0m #{pretty 0 id} = {#{ts'}}|]
        StringGlobal id var -> [i|\ESC[91mstrglo\ESC[0m #{pretty 0 (Id id)} = #{thow var}|]

data Arg = Argument Type Id deriving (Show, Data, Typeable)

instance Pretty Arg where
    pretty :: Int -> Arg -> Text
    pretty _ (Argument t id) = [i|#{pretty 0 t} #{pretty 0 id}|]

data LValue
    = LVar Id
    | LDeref Expr Int
    | LIndex Expr Expr
    | LStructIndex Expr Int
    deriving (Show, Data, Typeable)

instance Pretty LValue where
    pretty :: Int -> LValue -> Text
    pretty _ = \case
        LVar id -> pretty 0 id
        LDeref e int -> [i|#{pretty 0 e}->#{int}|]
        LIndex b index -> [i|#{pretty 0 b}[#{pretty 0 index}]|]
        LStructIndex e int -> [i|#{pretty 0 e}.#{int}|]

data Stmt
    = BStmt [Stmt] -- TODO: Remove
    | Decl Type Id
    | Ass Type LValue Expr
    | Ret (Maybe Expr)
    | CondElse Expr [Stmt] [Stmt]
    | Loop Expr [Stmt]
    | ArrayAlloc Type Id (Expr, Expr) -- (len, size)
    | SExp Expr
    | Break
    | ExtractFree Type Int Id
    | LoadSelf (Type, Id) (Type, Id)
    deriving (Show, Data, Typeable)

instance Pretty Stmt where
    pretty :: Int -> Stmt -> Text
    pretty n =
        indent n . \case
            BStmt bs -> "{\n" <> intercalate "\n" (map (semi (n + 1)) bs) <> "\n" <> indent n ("}" :: Text)
            Decl t i -> pretty 0 t <> " " <> pretty 0 i
            Ass ty l e -> "@" <> pretty 0 ty <> " " <> pretty 0 l <> " = " <> pretty 0 e
            Ret (Just e) -> "return " <> pretty 0 e
            Ret Nothing -> "return-void"
            CondElse e s1 s2 ->
                let els = "else" :: Text
                 in [i|if (#{pretty 0 e})\n#{pretty n (BStmt s1)}\n#{indent n els}\n#{pretty n (BStmt s2)}|]
            Loop e s ->
                [i|loop (#{pretty 0 e})
#{pretty (n + 1) (BStmt s)}|]
            ArrayAlloc ty name (_, si) -> [i|#{pretty n ty} #{pretty n name} = {alloc[#{pretty 0 si}]|]
            SExp e -> pretty 0 e
            Break -> "break"
            ExtractFree ty index id -> [i|@#{pretty n ty} #{pretty n id} = #{pretty 0 (Id "$captures$")}[#{index}]|]
            LoadSelf (_, fn) (_, name) -> [i|load-self {#{pretty n fn}} to {#{pretty n name}}|]

data Type
    = TVar Id
    | String
    | Double
    | Void
    | Boolean
    | Int
    | Fun Type [Type]
    | Pointer Type
    | Array Type
    | Closure Type
    deriving (Show, Data, Typeable)

instance Pretty Type where
    pretty :: Int -> Type -> Text
    pretty _ = \case
        TVar (Id i) -> i
        String -> "String"
        Double -> "F64"
        Void -> "void"
        Boolean -> "Bool"
        Int -> "I32"
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
    = EVar Id -- implemented
    | EGlobalVar Id
    | ELit Lit -- implemented
    | EApp Expr [Expr] -- implemented
    | Not Expr
    | Neg Expr
    | EMul Expr MulOp Expr -- implemented
    | EAdd Expr AddOp Expr -- implemented
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    | StructInit Bool [(Type, Lit)]
    | -- | Alloc the array and initialize all elements with the given expressions
      ArrayInit [Expr]
    | Cast Expr
    | Deref Expr Int
    | StructIndex Expr Int
    | ArrayIndex Expr Expr
    | ClosureLit Expr [Expr]
    deriving (Show, Data, Typeable)

instance Pretty Expr' where
    pretty :: Int -> Expr' -> Text
    pretty n =
        indent n . \case
            EVar i -> pretty 0 i
            EGlobalVar i -> "g\"" <> pretty 0 i
            ELit l -> pretty 0 l
            EApp id exp -> [i|#{pretty 0 id} (#{commaSeparated 0 exp})|]
            Not e -> "!" <> pretty 0 e
            Neg e -> "-" <> pretty 0 e
            EMul e1 op e2 -> [i|#{pretty 0 e1} #{pretty 0 op} #{pretty 0 e2}|]
            EAdd e1 op e2 -> [i|#{pretty 0 e1} #{pretty 0 op} #{pretty 0 e2}|]
            ERel e1 op e2 -> [i|#{pretty 0 e1} #{pretty 0 op} #{pretty 0 e2}|]
            EAnd e1 e2 -> [i|#{pretty 0 e1} && #{pretty 0 e2}|]
            EOr e1 e2 -> [i|#{pretty 0 e1} || #{pretty 0 e2}|]
            StructInit _ lits -> [i|{#{nice lits}}|]
              where
                nice :: [(Type, Lit)] -> Text
                nice = intercalate ", " . map (\(ty, lit) -> pretty 0 lit <> " : " <> pretty 0 ty)
            ArrayInit si -> [i|{#{intercalate ", " (map (pretty n) si)}}|]
            Cast c -> [i|cast (#{pretty 0 c})|]
            Deref e id -> [i|#{pretty 0 e}->#{id}|]
            StructIndex e id -> [i|#{pretty 0 e}.#{id}|]
            ArrayIndex b id -> [i|#{pretty 0 b}[#{pretty 0 id}]|]
            ClosureLit fun exprs ->
                let exprs' = intercalate ", " (map (pretty 0) exprs)
                 in [i|{#{pretty 0 fun} [#{exprs'}]}|]

bracket :: Text -> Text
bracket t = "[" <> t <> "]"

data Lit
    = LitInt Integer
    | LitDouble Double
    | LitBool Bool
    | LitString Text
    | LitNull
    | LitArrNull
    | LitFuncNull
    deriving (Show, Data, Typeable)

instance Pretty Lit where
    pretty :: Int -> Lit -> Text
    pretty _ = \case
        LitInt i -> thow i
        LitDouble d -> thow d
        LitBool b -> thow b
        LitString s -> thow s
        LitNull -> "null"
        LitArrNull -> "{ [], 0 }"
        LitFuncNull -> "{ null, null }"

{- Additive Operator -}
data AddOp
    = Plus
    | Minus
    deriving (Show, Data, Typeable)

instance Pretty AddOp where
    pretty :: Int -> AddOp -> Text
    pretty _ = \case
        Plus -> "+"
        Minus -> "-"

{- Multiplicative Operator -}
data MulOp
    = Times
    | Div
    | Mod
    deriving (Show, Data, Typeable)

instance Pretty MulOp where
    pretty :: Int -> MulOp -> Text
    pretty _ = \case
        Times -> "*"
        Div -> "/"
        Mod -> "%"

{-  Relational Operator -}
data RelOp
    = LTH
    | LE
    | GTH
    | GE
    | EQU
    | NE
    deriving (Show, Eq, Data, Typeable)

instance Pretty RelOp where
    pretty :: Int -> RelOp -> Text
    pretty _ = \case
        LTH -> "<"
        LE -> "<="
        GTH -> ">"
        GE -> ">="
        EQU -> "=="
        NE -> "!="

-- Identifier
newtype Id = Id Text
    deriving (Show, Eq, Ord, Data, Typeable)

instance Pretty Id where
    pretty _ (Id id) = indent 0 ("\ESC[93m" <> id <> "\ESC[0m")
