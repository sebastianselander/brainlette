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
    outputIr :: a -> Text

instance OutputIr Ir where
    outputIr :: Ir -> Text
    outputIr (Ir td) = unlines [outputIr td]

instance OutputIr [TopDef] where
    outputIr :: [TopDef] -> Text
    outputIr tds = intercalate "\n\n" $ map outputIr tds

instance (OutputIr a) => OutputIr (Maybe a) where
    outputIr = maybe "" outputIr

instance OutputIr TopDef where
    outputIr :: TopDef -> Text
    outputIr (ConstantString var cont) =
        concat
            [ "@"
            , var
            , " = private unnamed_addr constant"
            , outputIr (Array (length cont + 1) I8)
            , " "
            , "c\"" <> cont <> "\\00\""
            ]
    outputIr (Constant var typ cont) =
        concat
            [ "@"
            , var
            , " = private unnamed_addr constant "
            , outputIr typ
            , " "
            , outputIr cont
            ]
    outputIr (Declare t i args cconv) =
        unwords
            [ "declare"
            , outputIr t
            , "@" <> i
            , "(" <> outputIr args <> ")"
            , outputIr cconv
            ]
    outputIr (Define t i args cconv stmts) =
        unwords
            [ "define"
            , outputIr t
            , "@" <> i
            , "(" <> outputIr args <> ")"
            , outputIr cconv
            , "{\n"
            , "entry:"
            , outputIr stmts
            , "\n}"
            ]

indent :: Stmt -> Text
indent (Label {}) = ""
indent _ = "  "

instance OutputIr Stmt where
    outputIr :: Stmt -> Text
    outputIr s = indent s <> case s of
        VoidCall tail cconv t i args -> do
            let tail' = maybe "" (\t -> outputIr t <> " ") tail
            let t' = outputIr t
            let cconv' = maybe "" (\conv -> outputIr conv <> " ") cconv
            let args' = outputIr args
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
            let tail' = maybe "" (\t -> outputIr t <> " ") tail
            let t' = outputIr t
            let cconv' = maybe "" (\conv -> outputIr conv <> " ") cconv
            let args' = outputIr args
            let bind = outputIr var <> " = "
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
            "ret " <> case arg of
                Argument t i -> outputIr t <> " " <> outputIr i
                ConstArgument t i -> outputIr t <> " " <> outputIr i
        RetVoid -> "ret void"
        Comment t -> "; " <> t
        Arith var ar t a1 a2 ->
            concat
                [ outputIr var
                , " = "
                , outputIr ar
                , " "
                , outputIr t
                , " "
                , outputIr a1
                , ", "
                , outputIr a2
                ]
        Label text -> text <> ":"
        Alloca v t -> concat [outputIr v <> " = alloca ", outputIr t]
        Store val var -> concat ["store ", outputIr val, ", ptr ", outputIr var]
        Load var t ptr -> concat [outputIr var, " = load ", outputIr t, ", ptr ", outputIr ptr]
        Br cond l1 l2 -> concat ["br i1 ", outputIr cond, ", label %", l1, ", label %", l2]
        Jump l -> [i|br label %#{l}|]
        ICmp var op ty l r -> [i|#{outputIr var} = icmp #{outputIr op} #{outputIr ty} #{outputIr l}, #{outputIr r}|]
        FCmp var op ty l r -> [i|#{outputIr var} = fcmp #{outputIr op} #{outputIr ty} #{outputIr l}, #{outputIr r}|]
        Cast castop var t1 v2 t2 -> [i|%#{var} = #{outputIr castop} #{outputIr t1} %#{v2} to #{outputIr t2}|]
        And ty l r -> [i|and #{outputIr ty} #{outputIr l}, #{outputIr r}|]
        Or ty l r -> [i|or #{outputIr ty} #{outputIr l}, #{outputIr r}|]

instance OutputIr CastOp where
    outputIr :: CastOp -> Text
    outputIr = \case
        FPtoUI -> "fptoui"
        FPtoSI -> "fptosi"
        UItoFP -> "uitofp"
        SItoFP -> "sitofp"
        PTRtoINT -> "ptrtoint"
        INTtoPTR -> "inttoptr"
        Bitcast -> "bitcast"

instance OutputIr FCond where
    outputIr = \case 
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
    outputIr :: ICond -> Text
    outputIr = \case
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
    outputIr :: Arithmetic -> Text
    outputIr = \case
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
    outputIr :: [Stmt] -> Text
    outputIr st = "\n" <> intercalate "\n" (map outputIr st)

instance OutputIr Argument where
    outputIr :: Argument -> Text
    outputIr (ConstArgument Nothing i) = outputIr i
    outputIr (ConstArgument t i) = outputIr t <> " " <> outputIr i
    outputIr (Argument Nothing i) = outputIr i
    outputIr (Argument t i) = outputIr t <> " " <> outputIr i

instance OutputIr Variable where
    outputIr :: Variable -> Text
    outputIr (Variable t) = "%" <> t
    outputIr (ConstVariable t) = "@" <> t

instance OutputIr [Argument] where
    outputIr :: [Argument] -> Text
    outputIr as = intercalate ", " $ map outputIr as

instance OutputIr Lit where
    outputIr = \case
        LitInt n -> thow n
        LitDouble n -> thow n
        LitBool True -> "1"
        LitBool False -> "0"
        LitNull -> "null"

instance OutputIr Type where
    outputIr :: Type -> Text
    outputIr = \case
        I32 -> "i32"
        I8 -> "i8"
        F64 -> "double"
        Ptr -> "ptr"
        I1 -> "i1"
        Void -> "void"
        Array len t -> "[" <> thow len <> " x " <> outputIr t <> "]"
        FunPtr t ts -> outputIr t <> "(" <> outputIr ts <> ")*"
        CustomType t -> "%" <> t

instance OutputIr [Type] where
    outputIr :: [Type] -> Text
    outputIr ts = intercalate ", " $ map outputIr ts

instance OutputIr CallingConvention where
    outputIr :: CallingConvention -> Text
    outputIr = \case
        NoAttribute -> "noattribute"

instance OutputIr TailMarker where
    outputIr :: TailMarker -> Text
    outputIr = \case
        Tail -> "tail"
        MustTail -> "musttail"
        NoTail -> "notail"
