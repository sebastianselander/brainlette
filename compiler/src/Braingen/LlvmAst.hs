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
    = Argument (Maybe Type) Text
    | ConstArgument (Maybe Type) Lit
    deriving (Show)

newtype Ir = Ir [TopDef]
    deriving (Show)

data TopDef
    = Declare Type Text [Type] (Maybe CallingConvention)
    | Define Type Text [Argument] (Maybe CallingConvention) [Stmt]
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

data Condition
    = Eq
    | Ne
    | Ugt
    | Ult
    | Ule
    | Sgt
    | Sge
    | Slt
    | Sle
    deriving (Show)

type Variable = Text

type Label = Text

data Stmt
    = Call Variable (Maybe TailMarker) (Maybe CallingConvention) Type Text [Argument]
    | Arith Variable Arithmetic Type Argument Argument
    | ICmp Variable Condition Type Argument Argument
    | Alloca Variable Type
    | Store Argument Variable
    | Load Variable Type Variable
    | Ret Argument
    | RetVoid
    | Label Label
    | Comment Text
    | Br Variable Label Label
    | Jump Label
    deriving (Show)

data Lit
    = LitInt Integer
    | LitDouble Double
    | LitBool Bool
    | LitString Text
    deriving (Show)
