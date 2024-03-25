module TC.Error where

import TC.Types

data TcError 
    = UnboundVariable Ident
    | TypeMismatch Type [Type]
    deriving (Show, Eq, Ord)
