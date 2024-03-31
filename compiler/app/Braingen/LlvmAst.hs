module Braingen.LlvmAst where

import Data.Text (Text)

data CallingConvention
    = NoAttribute

data TailMarker = Tail | MustTail | NoTail

data Type
    = I32
    | Ptr
    | FunPtr Type [Type]
    | CustomType Text

data DebugInfo
    = DebugInfo
        { row :: Int
        , column :: Int
        , text :: Text
        }
    | NoInfo

data Argument
    = Argument Type Text
    | ConstArgument Type Text

newtype Ir = Ir [TopDef]

data TopDef
    = Declare Type Text [Type] CallingConvention
    | Define Type Text [Argument] CallingConvention [Stmt]

data Stmt
    = Call (Maybe TailMarker) (Maybe CallingConvention) Type Text [Argument]
    | Ret Argument
    | RetVoid
    | Comment Text
