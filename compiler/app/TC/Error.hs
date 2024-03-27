{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TC.Error where

import Data.List.NonEmpty (NonEmpty (..))
import Barser
import Ast
import ParserTypes
import Data.Text (Text, intercalate)

data TcError
    = -- | Constructor for an unbound variable error
      UnboundVariable
        -- | The source code position of the error
        Info
        -- | Name of the unbound variable
        IdSyn
    | -- | Constructor for mismatched types
      TypeMismatch
        -- | The source code position of the error
        Info
        -- | The given type
        TypeSyn
        -- | Expected types
        (NonEmpty TypeSyn)
    | -- | Constructor for when a function type was expected
      ExpectedFn
        -- | The source code position of the error
        Info
        -- | The given type
        TypeSyn
    | -- | Constructor for when a value of a type is non-comparable
      NotComparable
        -- | The source code position of the error
        Info
        -- | The given type
        TypeSyn
    | -- | Constructor for an illegal empty return
      IllegalEmptyReturn
        -- | The source code position of the error
        Info
        -- | The expected type
        TypeSyn
    deriving (Show)

class Report a where
    report :: a -> Text

instance Report Info where
    report _ = "info"

instance Report IdSyn where
    report _ = "idSyn"

instance Report TypeSyn where
    report = \case
           TVar _ id -> report id
           Fun _ rt argTys -> intercalate " ->" (map report (argTys ++ [rt]))

instance {-# OVERLAPPING #-} Report Text where
    report = id

instance (Report a) => Report [a] where
    report [] = ""
    report [x] = "'" <> report x <> "'"
    report (x : xs) = "'" <> report x <> "', " <> report xs

instance Report TcError where
        report _ = "tcerror"
--     report :: TcError -> String
--     report = \case
--         UnboundVariable pos (Id _ name) ->
--             "unbound variable '" <> name <> "' at " <> report pos
--         TypeMismatch pos given expected ->
--             "type '"
--                 <> report given
--                 <> "' does not match with "
--                 <> case expected of
--                     (x :| []) -> "'" <> report x <> "'"
--                     (x :| xs) -> "one of " <> report (x : xs)
--                 <> " at "
--                 <> report pos
--         ExpectedFn pos typ ->
--             "expected a function type, but got '"
--                 <> report typ
--                 <> "' at "
--                 <> report pos
--         NotComparable pos typ ->
--             "can not perform "
--                 <> report comps
--                 <> " on '"
--                 <> report typ
--                 <> "', at "
--                 <> report pos
--         IllegalEmptyReturn pos typ ->
--             "can not use empty return where a return type of '"
--                 <> report typ
--                 <> "' is expected, at "
--                 <> report pos
--
-- comps :: [String]
-- comps = ["==", "!=", "<=", ">=", "<", ">"]
