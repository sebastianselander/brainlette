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
    | -- | Constructor for when a value of a type is non-comparable
      NotComparable
        -- | The source code position of the error
        Position
        -- | The given type
        Type
    -- | Constructor for an illegal empty return
    | IllegalEmptyReturn
        -- | The source code position of the error
        Position
    deriving (Show, Eq, Ord)
