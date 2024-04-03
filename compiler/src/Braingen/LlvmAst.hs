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

type Variable = Text

type Label = Text

data Stmt
    = Call (Maybe TailMarker) (Maybe CallingConvention) Type Text [Argument]
    | Arith Arithmetic Type Argument Argument
    | Alloca Variable Type
    | Store Argument Variable
    | Load Variable Type Variable
    | Ret Argument
    | RetVoid
    | Label Label
    | Comment Text
    deriving (Show)
