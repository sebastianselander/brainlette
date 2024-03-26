{-# LANGUAGE LambdaCase #-}

module TC.Error where

import Data.List.NonEmpty (NonEmpty (..))
import TC.Types
import Data.List (intercalate)

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
        (NonEmpty Type)
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
    | -- | Constructor for an illegal empty return
      IllegalEmptyReturn
        -- | The source code position of the error
        Position
        -- | The expected type
        Type
    deriving (Show, Eq, Ord)

class Report a where
    report :: a -> String

instance Report Position where
    report Nothing = "unknown position"
    report (Just (row, col)) = show row <> ":" <> show col

instance Report Type where
    report = \case
           Int _ -> "int"
           Double _ -> "double"
           Bool _ -> "boolean"
           String _ -> "string"
           Void _ -> "void"
           Fun _ rt argTys -> intercalate " ->" (map report (argTys ++ [rt]))

instance {-# OVERLAPPING #-} Report String where
    report = id

instance (Report a) => Report [a] where
    report [] = ""
    report [x] = "'" <> report x <> "'"
    report (x : xs) = "'" <> report x <> "', " <> report xs

instance Report TcError where
    report :: TcError -> String
    report = \case
        UnboundVariable pos (Ident name) ->
            "unbound variable '" <> name <> "' at " <> report pos
        TypeMismatch pos given expected ->
            "type '"
                <> report given
                <> "' does not match with "
                <> case expected of
                    (x :| []) -> "'" <> report x <> "'"
                    (x :| xs) -> "one of " <> report (x : xs)
                <> " at "
                <> report pos
        ExpectedFn pos typ ->
            "expected a function type, but got '"
                <> report typ
                <> "' at "
                <> report pos
        NotComparable pos typ ->
            "can not perform "
                <> report comps
                <> " on '"
                <> report typ
                <> "', at "
                <> report pos
        IllegalEmptyReturn pos typ ->
            "can not use empty return where a return type of '"
                <> report typ
                <> "' is expected, at "
                <> report pos

comps :: [String]
comps = ["==", "!=", "<=", ">=", "<", ">"]
