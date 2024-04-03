{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TypeChecker.Error where

import Data.List.NonEmpty (NonEmpty (..))
import Data.String.Interpolate
import Data.Text (Text, cons, intercalate, unlines)
import ParserTypes (SynInfo (..))
import TypeChecker.Types
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
    | -- | Constructor for an already bound variable
      BoundVariable
        -- | The source code position of the error
        SynInfo
        -- | Name of the bound variable
        Id
    deriving (Show)

class Report a where
    report :: a -> Text

instance Report SynInfo where
    report NoInfo = ""
    report info =
        [iii|#{star} In '#{sourceCode info}'
                     \n#{star} At #{sourceLine info}:#{sourceColumn info}
                     \n#{star} In the module '#{sourceName info}'|]

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
            pretty $ combine [i|Unbound variable'#{name}'|] info
        TypeMismatch info given expected ->
            let one = case expected of
                    (x :| []) -> "'" <> report x <> "'"
                    (x :| xs) -> "one of " <> report (x : xs)
             in pretty $ combine [i|Type '#{report given}' does not match with #{one}|] info
        ExpectedFn pos typ ->
            pretty $
                combine
                    [i|Expected a function type, but go '#{report typ}'|]
                    pos
        NotComparable info op typ ->
            pretty $
                combine
                    [i|Can't perform '#{report op}' on tyoe '#{report typ}'|]
                    info
        IllegalEmptyReturn pos typ ->
            pretty $
                combine
                    [i|Can't use empty return where a return type of '#{report typ}' is expected|]
                    pos
        ExpectedType info expected given ->
            pretty $
                combine
                    [i|Expected type '#{report expected}' but go '#{report given}'|]
                    info
        ExpectedNumber info ty ->
            pretty $
                combine
                    [i|Expected a numeric type, but got '#{report ty}'|]
                    info
        BoundVariable info id ->
            pretty $
                combine
                    [i|Variable '#{report id}' already declared earlier|]
                    info

quote :: Text -> Text
quote s = "'" <> s <> "'"

pretty :: [Text] -> Text
pretty [] = ""
pretty (x : xs) = unlines (bold x : xs)

-- TODO: Implement
bold :: Text -> Text
bold s = s

combine :: (Report a) => Text -> a -> [Text]
combine xs info = (star `cons` ' ' `cons` xs) : [report info]

star :: Char
star = '•'
