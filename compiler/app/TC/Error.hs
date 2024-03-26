module TC.Error where

import TC.Types

data TcError
    = UnboundVariable Position Ident
    | TypeMismatch Position Type [Type]
    | ExpectedFn Position Type
    deriving (Show, Eq, Ord)
