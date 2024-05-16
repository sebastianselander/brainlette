{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Braingen.Output where

import Braingen.LlvmAst
import Data.String.Interpolate
import Data.Text (Text, concat, intercalate, length, unlines, unwords)
import Utils (thow)
import Prelude hiding (concat, length, unlines, unwords)

class OutputIr a where
    out :: a -> Text

instance OutputIr Ir where
    out :: Ir -> Text
    out (Ir td) = unlines [out td]

instance OutputIr [TopDef] where
    out :: [TopDef] -> Text
    out tds = intercalate "\n\n" $ map out tds

instance (OutputIr a) => OutputIr (Maybe a) where
    out = maybe "" out

instance OutputIr TopDef where
    out :: TopDef -> Text
    out (ConstantString var cont) =
        concat
            [ "@"
            , var
            , " = private unnamed_addr constant"
            , out (Array (length cont + 1) I8)
            , " "
            , "c\"" <> cont <> "\\00\""
            ]
    out (Constant var typ cont) =
        concat
            [ "@"
            , var
            , " = private unnamed_addr constant "
            , out typ
            , " "
            , out cont
            ]
    out (Declare t i args cconv) =
        unwords
            [ "declare"
            , out t
            , "@" <> i
            , "(" <> out args <> ")"
            , out cconv
            ]
    out (Define t i args cconv stmts) =
        unwords
            [ "define"
            , out t
            , "@" <> i
            , "(" <> out args <> ")"
            , out cconv
            , "{"
            , out stmts
            , "\n}"
            ]
    out (Type t ts) = do
        let types = intercalate ", " $ map out ts
        concat
            [ "%"
            , t
            , " = type { "
            , types
            , " }"
            ]

indent :: Stmt -> Text
indent (Label {}) = ""
indent _ = "  "

instance OutputIr Stmt where
    out :: Stmt -> Text
    out = \case
        VoidCall tail cconv t i args -> do
            let tail' = maybe "" (\t -> out t <> " ") tail
            let t' = out t
            let cconv' = maybe "" (\conv -> out conv <> " ") cconv
            let args' = out args
            concat
                [ tail'
                , "call "
                , cconv'
                , t'
                , " @"
                , i
                , "("
                , args'
                , ")"
                ]
        Call var tail cconv t i args -> do
            let tail' = maybe "" (\t -> out t <> " ") tail
            let t' = out t
            let cconv' = maybe "" (\conv -> out conv <> " ") cconv
            let args' = out args
            let bind = out var <> " = "
            concat
                [ bind
                , tail'
                , "call "
                , cconv'
                , t'
                , " @"
                , i
                , "("
                , args'
                , ")"
                ]
        Ret arg ->
            "ret " <> out arg
        RetVoid -> "ret void"
        Comment t -> "; " <> t
        Arith var ar t a1 a2 ->
            concat
                [ out var
                , " = "
                , out ar
                , " "
                , out t
                , " "
                , out a1
                , ", "
                , out a2
                ]
        Label text -> text <> ":"
        Alloca v t -> concat [out v <> " = alloca ", out t]
        Malloc v s -> concat [out v <> " = call ptr @malloc(i64 ", out s, ")"]
        Store val var -> concat ["store ", out val, ", ptr ", out var]
        Load var t ptr -> concat [out var, " = load ", out t, ", ptr ", out ptr]
        Br cond l1 l2 -> concat ["br i1 ", out cond, ", label %", l1, ", label %", l2]
        Jump l -> [i|br label %#{l}|]
        ICmp var op ty l r -> [i|#{out var} = icmp #{out op} #{out ty} #{out l}, #{out r}|]
        FCmp var op ty l r -> [i|#{out var} = fcmp #{out op} #{out ty} #{out l}, #{out r}|]
        Cast castop var t1 v2 t2 -> [i|#{out var} = #{out castop} #{out t1} #{out v2} to #{out t2}|]
        And var ty l r -> [i|#{out var} = and #{out ty} #{out l}, #{out r}|]
        Or var ty l r -> [i|#{out var} = or #{out ty} #{out l}, #{out r}|]
        Fneg var ty arg -> [i|#{out var} = fneg #{out ty} #{out arg}|]
        Unreachable -> "unreachable"
        GetElementPtr var ty arg1 arg2 -> [i|#{out var} = getelementptr #{out ty}, #{out arg1}, i64 0, #{out arg2}|]
        ExtractValue var ty arg ind -> [i|#{out var} = extractvalue #{out ty} #{out arg}, #{ind}|]

instance OutputIr CastOp where
    out :: CastOp -> Text
    out = \case
        FPtoUI -> "fptoui"
        FPtoSI -> "fptosi"
        UItoFP -> "uitofp"
        SItoFP -> "sitofp"
        PTRtoINT -> "ptrtoint"
        INTtoPTR -> "inttoptr"
        Bitcast -> "bitcast"

instance OutputIr FCond where
    out = \case
        Ffalse -> "false"
        Foeq -> "oeq"
        Fogt -> "ogt"
        Foge -> "oge"
        Folt -> "olt"
        Fole -> "ole"
        Fone -> "one"
        Ford -> "ord"
        Fueq -> "ueq"
        Fugt -> "ugt"
        Fuge -> "uge"
        Fult -> "ult"
        Fule -> "ule"
        Fune -> "une"
        Funo -> "uno"
        Ftrue -> "true"

instance OutputIr ICond where
    out :: ICond -> Text
    out = \case
        Ieq -> "eq"
        Ine -> "ne"
        Iugt -> "ugt"
        Iult -> "ult"
        Iule -> "ule"
        Isgt -> "sgt"
        Isge -> "sge"
        Islt -> "slt"
        Isle -> "sle"

instance OutputIr Arithmetic where
    out :: Arithmetic -> Text
    out = \case
        Add -> "add"
        Sub -> "sub"
        FAdd -> "fadd"
        FSub -> "fsub"
        Mul -> "mul"
        FMul -> "fmul"
        UDiv -> "udiv"
        SDiv -> "sdiv"
        FDiv -> "fdiv"
        URem -> "urem"
        FRem -> "frem"

instance OutputIr [Stmt] where
    out :: [Stmt] -> Text
    out st = "\n" <> intercalate "\n" (map (\s -> indent s <> out s) st)

instance OutputIr Argument where
    out :: Argument -> Text
    out (ConstArgument t i) = out t <> " " <> out i
    out (Argument t i) = out t <> " " <> out i
    out (ConstArgumentAuto l) =
        let ty = case l of
                LitInt _ -> I64
                LitDouble _ -> F64
                LitBool _ -> I1
                LitNull -> Ptr
                LitArrNull -> Ptr
         in out (ConstArgument (pure ty) l)

instance OutputIr Variable where
    out :: Variable -> Text
    out (Variable t) = "%" <> t
    out (ConstVariable t) = "@" <> t

instance OutputIr [Argument] where
    out :: [Argument] -> Text
    out as = intercalate ", " $ map out as

instance OutputIr Lit where
    out :: Lit -> Text
    out = \case
        LitInt n -> thow n
        LitDouble n -> thow n
        LitBool True -> "1"
        LitBool False -> "0"
        LitNull -> "null"
        LitArrNull -> "{ ptr null, i64 0}"

instance OutputIr Type where
    out :: Type -> Text
    out = \case
        I64 -> "i64"
        I32 -> "i32"
        I8 -> "i8"
        F64 -> "double"
        Ptr -> "ptr"
        I1 -> "i1"
        Void -> "void"
        Array len t -> "[" <> thow len <> " x " <> out t <> "]"
        FunPtr t ts -> out t <> "(" <> out ts <> ")*"
        CustomType t -> "%" <> t
        RawPtr t -> out t <> "*"

instance OutputIr [Type] where
    out :: [Type] -> Text
    out ts = intercalate ", " $ map out ts

instance OutputIr CallingConvention where
    out :: CallingConvention -> Text
    out = \case
        NoAttribute -> "noattribute"

instance OutputIr TailMarker where
    out :: TailMarker -> Text
    out = \case
        Tail -> "tail"
        MustTail -> "musttail"
        NoTail -> "notail"
