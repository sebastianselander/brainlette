module Braingen.LlvmAst where

import Data.Text (Text)

data Ident a = Ident a Text

data FunctionAttribute
    = NoAttribute

data Type
    = I32
    | Ptr

data TopDef a
    = Declare a Type (Ident a) FunctionAttribute
    | Define a Type (Ident a) FunctionAttribute [Stms a]

data Stms a
    = Call a Type (Ident a) -- TODO