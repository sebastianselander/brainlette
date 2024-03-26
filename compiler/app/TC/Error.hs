module TC.Error where

import TC.Types

data TcError
    = -- | Constructor for an unbound variable error
      UnboundVariable
        -- | The source code position of the error
        Position
        -- | Name of the unbound variable
        Ident
    | -- | Constructor for mismatched types
      TypeMismatch
        -- | The source code position of the error
        Position
        -- | The given type
        Type
        -- | Expected types
        [Type]
    | -- | Constructor for when a function type was expected
      ExpectedFn
        -- | The source code position of the error
        Position
        -- | The given type
        Type
    deriving (Show, Eq, Ord)
