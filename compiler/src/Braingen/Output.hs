{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Braingen.Output where

import Braingen.LlvmAst
import Data.Text (Text, concat, intercalate, unwords)
import Prelude hiding (concat, unwords)

class OutputIr a where
    outputIr :: a -> Text

instance OutputIr Ir where
    outputIr :: Ir -> Text
    outputIr (Ir td) = outputIr td

instance OutputIr [TopDef] where
    outputIr :: [TopDef] -> Text
    outputIr tds = intercalate "\n\n" $ map outputIr tds

instance OutputIr TopDef where
    outputIr :: TopDef -> Text
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
        Call tail cconv t i args -> do
            let tail' = case tail of
                    Just t -> outputIr t <> " "
                    Nothing -> ""
            let t' = outputIr t
            let cconv' = case cconv of
                    Just c -> outputIr c <> " "
                    Nothing -> ""
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
        Ret arg ->
            "ret " <> case arg of
                Argument t i -> outputIr t <> " %" <> i
                ConstArgument t i -> outputIr t <> " " <> i
        RetVoid -> "ret void"
        Comment t -> "; " <> t

instance OutputIr [Stmt] where
    outputIr :: [Stmt] -> Text
    outputIr st = "\n  " <> intercalate "\n  " (map outputIr st)

instance OutputIr Argument where
    outputIr :: Argument -> Text
    outputIr (ConstArgument t i) = outputIr t <> " " <> i
    outputIr (Argument t i) = outputIr t <> " %" <> i

instance OutputIr [Argument] where
    outputIr :: [Argument] -> Text
    outputIr as = intercalate ", " $ map outputIr as

instance OutputIr Type where
    outputIr :: Type -> Text
    outputIr = \case
        I32 -> "i32"
        Ptr -> "ptr"
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
