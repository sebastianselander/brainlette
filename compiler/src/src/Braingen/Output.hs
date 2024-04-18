{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Braingen.Output where

import Braingen.LlvmAst
import Data.String.Interpolate
import Data.Text (Text, concat, intercalate, length, unlines, unwords)
import Utils (thow)
import Prelude hiding (concat, length, unlines, unwords)

defaultStart :: Text
defaultStart =
    unlines
        [ "target triple = \"x86_64-pc-linux-gnu\"\n"
        , "target datalayout = \"e-m:o-i64:64-f80:128-n8:16:32:64-S128\"\n"
        ]

printfDecl :: Text
printfDecl = "declare i32 @printf(ptr nocapture, ...) nounwind"

readInt :: Text
readInt =
    unlines
        [ "define i32 @readInt ()  {"
        , "  ret i32 0"
        , "}"
        ]

readDouble :: Text
readDouble =
    unlines
        [ "define double @readDouble ()  {"
        , "  ret double 0.0"
        , "}"
        ]

printString :: Text
printString =
    unlines
        [ "@.printString = private unnamed_addr constant [4 x i8] c\"%s\\0A\\00\""
        , "define void @printString (ptr %arg)  {"
        , "  call i32 (...) @printf(ptr @.printString, ptr %arg)"
        , "  ret void"
        , "}"
        ]

printDouble :: Text
printDouble =
    unlines
        [ "@.printDouble = private unnamed_addr constant [4 x i8] c\"%g\\0A\\00\""
        , "define void @printDouble (double %arg)  {"
        , "  call i32 (...) @printf(ptr @.printDouble, double %arg)"
        , "  ret void"
        , "}"
        ]

printInt :: Text
printInt =
    unlines
        [ "@.printInt = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\""
        , "define void @printInt (i32 %arg)  {"
        , "  call i32 (...) @printf(ptr @.printInt, i32 %arg)"
        , "  ret void"
        , "}"
        ]

prelude :: Text
prelude = unlines [readInt, readDouble, printInt, printDouble, printString]

class OutputIr a where
    outputIr :: a -> Text

instance OutputIr Ir where
    outputIr :: Ir -> Text
    outputIr (Ir td) = defaultStart <> unlines [printfDecl, outputIr td] <> prelude

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
            , "c\"" <> cont <> "\00\""
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
            , "{"
            , outputIr stmts
            , "\n}"
            ]

instance OutputIr Stmt where
    outputIr :: Stmt -> Text
    outputIr = \case
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
        ICmp var op ty l r -> [i|#{var} = #{outputIr op} #{outputIr ty} #{outputIr l} #{outputIr r}|]
        Cast castop var t1 v2 t2 -> [i|%#{var} = #{outputIr castop} #{outputIr t1} %#{v2} to #{outputIr t2}|]

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

instance OutputIr Condition where
    outputIr :: Condition -> Text
    outputIr = \case
        Eq -> "eq"
        Ne -> "ne"
        Ugt -> "ugt"
        Ult -> "ult"
        Ule -> "ule"
        Sgt -> "sgt"
        Sge -> "sge"
        Slt -> "slt"
        Sle -> "sle"

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
    outputIr st = "\n  " <> intercalate "\n  " (map outputIr st)

instance OutputIr Argument where
    outputIr :: Argument -> Text
    outputIr (ConstArgument t i) = outputIr t <> " " <> outputIr i
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
