module Braingen.LlvmAst where

import Data.Text (Text)

data CallingConvention
    = NoAttribute
    deriving (Show)

data TailMarker = Tail | MustTail | NoTail
    deriving (Show)

data Type
    = I32
    | I64
    | I1
    | I8
    | F64
    | Ptr
    | RawPtr Type
    | Void
    | FunPtr Type [Type]
    | Array Int Type
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
    = Argument (Maybe Type) Variable
    | ConstArgument (Maybe Type) Lit
    deriving (Show)

newtype Ir = Ir [TopDef]
    deriving (Show)

data TopDef
    = Declare Type Text [Type] (Maybe CallingConvention)
    | Define Type Text [Argument] (Maybe CallingConvention) [Stmt]
    | Constant Text Type Lit
    | ConstantString Text Text
    | Type Text [Type]
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

data FCond
    = Ffalse
    | Foeq
    | Fogt
    | Foge
    | Folt
    | Fole
    | Fone
    | Ford
    | Fueq
    | Fugt
    | Fuge
    | Fult
    | Fule
    | Fune
    | Funo
    | Ftrue
    deriving (Show)

data ICond
    = Ieq
    | Ine
    | Iugt
    | Iult
    | Iule
    | Isgt
    | Isge
    | Islt
    | Isle
    deriving (Show)

data CastOp
    = FPtoUI
    | FPtoSI
    | UItoFP
    | SItoFP
    | PTRtoINT
    | INTtoPTR
    | Bitcast
    deriving (Show)

data Variable = Variable Text | ConstVariable Text
    deriving (Show)

type Label = Text

data Stmt
    = Call Variable (Maybe TailMarker) (Maybe CallingConvention) Type Text [Argument]
    | VoidCall (Maybe TailMarker) (Maybe CallingConvention) Type Text [Argument]
    | Arith Variable Arithmetic Type Argument Argument
    | ICmp Variable ICond Type Argument Argument
    | FCmp Variable FCond Type Argument Argument
    | And Variable Type Argument Argument
    | Fneg Variable Type Argument
    | Or Variable Type Argument Argument
    | GetElementPtr Variable Type Argument Argument
    | Alloca Variable Type
    | Malloc Variable Integer
    | Store Argument Variable
    | Load Variable Type Variable
    | Ret Argument
    | RetVoid
    | Label Label
    | Comment Text
    | Br Variable Label Label
    | Jump Label
    | Cast CastOp Variable Type Variable Type
    | Unreachable
    deriving (Show)

data Lit
    = LitInt Integer
    | LitDouble Double
    | LitBool Bool
    | LitNull
    deriving (Show)
