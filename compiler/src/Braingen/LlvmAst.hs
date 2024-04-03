module Braingen.LlvmAst where

import Data.Text (Text)

data CallingConvention
    = NoAttribute
    deriving (Show)

data TailMarker = Tail | MustTail | NoTail
    deriving (Show)

data Type
    = I32
    | I1
    | F64
    | Ptr
    | FunPtr Type [Type]
    | CustomType Text
    deriving (Show)

data DebugInfo
    = DebugInfo
        { row :: Int
        , column :: Int
        , text :: Text
        }
    | NoInfo
    deriving (Show)

data Argument
    = Argument Type Text
    | ConstArgument Type Text
    deriving (Show)

newtype Ir = Ir [TopDef]
    deriving (Show)

data TopDef
    = Declare Type Text [Type] CallingConvention
    | Define Type Text [Argument] CallingConvention [Stmt]
    deriving (Show)

data Arithmetic
    = Add
    | Sub
    | FAdd
    | FSub
    | Mul
    | FMul
    | UDiv
    | SDiv
    | FDiv
    | URem
    | FRem
    deriving (Show)

data Stmt
    = Call (Maybe TailMarker) (Maybe CallingConvention) Type Text [Argument]
    | Arith Arithmetic Type Argument Argument
    | Ret Argument
    | RetVoid
    | Label Text
    | Comment Text
    deriving (Show)
