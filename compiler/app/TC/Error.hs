module TC.Error where

import Brainlette.Abs (BNFC'Position)
import TC.Types

type Position = BNFC'Position

data TcError
    = UnboundVariable Position Ident
    | TypeMismatch Position Type [Type]
    deriving (Show, Eq, Ord)
