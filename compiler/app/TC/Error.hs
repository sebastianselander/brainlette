{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TC.Error where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, cons, intercalate, pack, unlines, unwords)
import ParserTypes (SynInfo (..))
import TC.Types
import Prelude hiding (unlines, unwords)

data TcError
    = -- | Constructor for an unbound variable error
      UnboundVariable
        -- | The source code position of the error
        SynInfo
        -- | Name of the unbound variable
        Id
    | -- | Constructor for mismatched types
      TypeMismatch
        -- | The source code position of the error
        SynInfo
        -- | The given type
        Type
        -- | Expected types
        (NonEmpty Type)
    | -- | Constructor for when a function type was expected
      ExpectedFn
        -- | The source code position of the error
        SynInfo
        -- | The given type
        Type
    | -- | Constructor for when a value of a type is non-comparable
      NotComparable
        -- | The source code position of the error
        SynInfo
        -- | Comparison operator
        RelOp
        -- | The given type
        Type
    | -- | Constructor for an illegal empty return
      IllegalEmptyReturn
        -- | The source code position of the error
        SynInfo
        -- | The expected type
        Type
    | -- | Constructor for when the expected type was not given
      ExpectedType
        -- | The source code position of the error
        SynInfo
        -- | The expected type
        Type
        -- | The given type
        Type
    | -- | Constructor for when a number is expected
      ExpectedNumber
        -- | The source code position of the error
        SynInfo
        -- | The given type
        Type
    deriving (Show)

class Report a where
    report :: a -> Text

instance Report SynInfo where
    report i =
        unlines
            [ cons star " In '" <> i.sourceCode <> "'"
            , cons star " At " <> pack (show i.sourceLine) <> ":" <> pack (show i.sourceColumn)
            , cons star " In the module " <> quote i.sourceName
            ]

instance Report Id where
    report (Id a) = a

instance Report Type where
    report = \case
        TVar id -> report id
        Fun rt argTys -> intercalate " ->" (map report (argTys ++ [rt]))

instance {-# OVERLAPPING #-} Report Text where
    report = id

instance (Report a) => Report [a] where
    report [] = ""
    report [x] = "'" <> report x <> "'"
    report (x : xs) = "'" <> report x <> "', " <> report xs

instance Report RelOp where
    report = \case
        LTH -> "<"
        LE -> "<="
        GTH -> ">"
        GE -> ">="
        EQU -> "=="
        NE -> "!="

instance Report TcError where
    report = \case
        UnboundVariable info (Id name) ->
            pretty $ combine ["Unbound variable '" <> name <> "'"] info
        TypeMismatch pos given expected ->
            pretty $ combine
                [ "Type "
                    <> quote (report given)
                    <> " does not match with "
                    <> quote
                        ( case expected of
                            (x :| []) -> "'" <> report x <> "'"
                            (x :| xs) -> "one of " <> report (x : xs)
                        )
                ]
                pos
        ExpectedFn pos typ ->
            pretty $ combine
                [ "Expected a function type, but got "
                    <> quote (report typ)
                ]
                pos
        NotComparable info op typ ->
            pretty $ combine
                [ "Can't perform "
                    <> quote (report op)
                    <> " on type "
                    <> quote (report typ)
                ]
                info
        IllegalEmptyReturn pos typ ->
            pretty $ combine
                [ "Can not use empty return where a return type of "
                    <> quote (report typ)
                    <> " is expected"
                ]
                pos
        ExpectedType info expected given ->
            pretty $ combine
                [unwords ["Expected type", quote (report expected), "but got", quote (report given)]]
                info
        ExpectedNumber info ty ->
            pretty $ combine ["Expected a numeric type, but got " <> quote (report ty)] info

quote :: Text -> Text
quote s = "'" <> s <> "'"

pretty :: [Text] -> Text
pretty [] = ""
pretty (x:xs) = unlines (bold x : xs)

-- TODO: Implement
bold :: Text -> Text
bold s = s

combine :: (Report a) => [Text] -> a -> [Text]
combine xs info = go xs <> [report info]
  where
    go :: [Text] -> [Text]
    go [] = []
    go (x : xs) = cons star (cons ' ' x) : go xs

star :: Char
star = '•'
