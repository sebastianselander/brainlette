{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Frontend.Error where

import Data.List.NonEmpty (NonEmpty (..))
import Data.String.Interpolate
import Data.Text (Text, cons, pack, takeWhile, unlines)
import Frontend.Parser.BrainletteParser (hasInfo)
import Frontend.Parser.ParserTypes (SynInfo (..))
import Frontend.Parser.ParserTypes qualified as Par
import Frontend.Tc.Types
import GHC.Generics (Constructor)
import Prelude hiding (takeWhile, unlines, unwords)

data FEError
    = -- | Constructor for an unbound variable error
      UnboundVariable
        -- | The source code position of the error
        SynInfo
        -- | Name of the unbound variable
        Id
    | -- | Constructor for when a type can't be inferred
      TypeUninferrable
        -- | The source code position of the error
        SynInfo
        -- | The given expression
        Par.Expr
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
    | -- | Constructor for break statement outside a loop
      BreakNotInLoop
        -- | The source code position of the error
        SynInfo
    | -- | Constructor for unreachable statements outside a loop
      UnreachableStatement
        -- | The source code position of the error
        SynInfo
    | -- | Constructor for functions missing a return statement
      MissingReturn
        -- | The source code position of the error
        SynInfo
        -- | The function missing return
        Par.Id
    | -- | Constructor for an expression that is not a statement
      NotStatement
        -- | The source code position of the error
        SynInfo
        -- | The expression that is not a statement
        Par.Expr
    | -- | Constructor for a function call that has a mismatched number of argument
      ArgumentMismatch
        -- | The source code position of the error
        SynInfo
        -- | The expression that has an invalid amount of arguments
        Par.Id
        -- | Expected amount of arguments
        Int
        -- | Given amount of arguments
        Int
    | -- | Constructor for duplicate top definitions
      DuplicateTopDef
        -- | The source code position of the error
        SynInfo
        -- | The duplicate top definition
        Par.TopDef
    | -- | Constructor for a duplicate error
      DuplicateArgument
        -- | The source code position of the error
        SynInfo
        -- | The duplicate argument
        Par.Arg
    | -- | Constructor for declaring a void variable
      VoidDeclare
        -- | The source code position of the error
        SynInfo
        -- | The bogus statement
        Par.Stmt
    | -- | Constructor for having parameters of type void
      VoidParameter
        -- | The source code position of the error
        SynInfo
        -- | Name of the function
        Par.Id
    | -- | Constructor for types that can not be compared
      NotRelational
        -- | The source code position of the error
        SynInfo
        -- | The type that is not relational
        Type
    | -- | Constructor for an unknown struct
      UnboundStruct
        -- | The source code position of the error
        SynInfo
        -- | Name of the struct
        Id
    | -- | Constructor for an unbound field
      UnboundField
        -- | The source code position of the error
        SynInfo
        -- | The unbound field
        Par.Id
    | -- | Constructor for field lookup on an incompatible type
      NotFieldType
        -- | The source code position of the error
        SynInfo
        -- | Name of the struct
        Type
    | -- | Constructor for a type that is not a pointer
      NotPointer
        -- | The source code position of the error
        SynInfo
        -- | The type that is not a pointer
        Type
    | -- | Constructor for when an identifer was expected
      ExpectedIdentifier
        -- | The source code position of the error
        SynInfo
        -- | The expression that is not an identifier
        Par.Expr
    | -- | Constructor for an expression that is not an lvalue
      NotLValue
        -- | The source code position of the error
        SynInfo
        -- | The expression that is not an LValue
        Par.Expr
    | -- | Constructor for circluar typedefs
      TypeDefCircular
        -- | The type that was circular
        Type
    | UnboundType
        -- | The source code position of the error
        SynInfo
        -- | The type that does not exist
        Type
    deriving (Show)

parens :: Text -> Text
parens s = "(" <> s <> ")"

brackets :: Text -> Text
brackets s = s <> "[]"

class Report a where
    report :: a -> Text

instance Report Int where
    report = pack . show

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
        String -> "string"
        Int -> "int"
        Double -> "double"
        Void -> "void"
        Boolean -> "boolean"
        TVar id -> report id
        Fun rt argTys -> report rt <> parens (report argTys)
        Pointer ty -> report ty <> "*"

instance Report Text where
    report = id

instance (Report a) => Report [a] where
    report [] = ""
    report [x] = report x
    report (x : xs) = report x <> ", " <> report xs

instance Report RelOp where
    report = \case
        LTH -> "<"
        LE -> "<="
        GTH -> ">"
        GE -> ">="
        EQU -> "=="
        NE -> "!="

instance Report FEError where
    report = \case
        UnboundVariable info (Id name) ->
            pretty $ combine info [i|Unbound variable '#{name}'|]
        TypeMismatch info given expected ->
            let one = case expected of
                    (x :| []) -> "'" <> report x <> "'"
                    (x :| xs) -> "one of " <> report (x : xs)
             in pretty $ combine info [i|Type '#{report given}' does not match with #{one}|]
        ExpectedFn info typ ->
            pretty $
                combine
                    info
                    [i|Expected a function type, but got '#{report typ}'|]
        NotComparable info op typ ->
            pretty $
                combine
                    info
                    [i|Can't perform '#{report op}' on tyoe '#{report typ}'|]
        IllegalEmptyReturn info typ ->
            pretty $
                combine
                    info
                    [i|Can't use empty return where a return type of '#{report typ}' is expected|]
        ExpectedType info expected given ->
            pretty $
                combine
                    info
                    [i|Expected type '#{report expected}' but got '#{report given}'|]
        ExpectedNumber info ty ->
            pretty $
                combine
                    info
                    [i|Expected a numeric type, but got '#{report ty}'|]
        BoundVariable info id ->
            pretty $
                combine
                    info
                    [i|Variable '#{report id}' already declared earlier|]
        BreakNotInLoop info ->
            [i|break outside loop\n#{sourceCode info}\n#{sourceLine info}:#{sourceColumn info}|]
        UnreachableStatement info -> pretty $ combine (oneLine info) [i|unreachable statement|]
        MissingReturn info name -> pretty $ combine (oneLine info) [i|missing return in function #{report name}|]
        NotStatement info _ -> pretty $ combine info [i|The expression is not a statement|]
        ArgumentMismatch info name expected given ->
            pretty $
                combine
                    info
                    [i|in the call to the function '#{report name}', #{report expected} arguments were expected, but got #{report given} |]
        DuplicateTopDef info tp -> pretty $ combine info [i|duplicate top definition\n#{report tp}|]
        DuplicateArgument info tp -> pretty $ combine info [i|duplicate argument in definition\n#{report tp}|]
        VoidDeclare info _ -> pretty $ combine info [i|can not declare a variable with type '#{report Void}'|]
        NotRelational info ty -> pretty $ combine info [i|can not perform relational operations on #{report ty}|]
        VoidParameter info ident ->
            pretty $
                combine info [i|can not have parameters of type '#{report Void}' in the function #{report ident}|]
        TypeUninferrable info _expr -> pretty $ combine info [i|can't infer type of expression TODO: show expression|]
        UnboundStruct info name -> pretty $ combine info [i|unknown struct '#{report name}'|]
        UnboundField info name -> pretty $ combine info [i|unknown struct field '#{report name}'|]
        NotFieldType info ty -> pretty $ combine info [i|expected a struct type, but got '#{report ty}'|]
        NotPointer info ty -> pretty $ combine info [i|can not access a struct field on the non-pointer type '#{report ty}'|]
        ExpectedIdentifier info _expr -> pretty $ combine info [i|expected an identifier, not an expression TODO: show expression|]
        NotLValue info _expr -> pretty $ combine info [i|lhs must be an lvalue, got TODO: show expression|]
        TypeDefCircular ty -> pretty $ combine NoInfo [i| circular typedef found for '#{report ty}'|]
        UnboundType info ty -> pretty $ combine info [i|unbound type '#{report ty}'|]


oneLine :: SynInfo -> SynInfo
oneLine info = info {sourceCode = takeWhile (/= '\n') info.sourceCode}

errMissingRet :: Par.TopDef -> Text
errMissingRet (Par.FnDef info _ _ _ stmts) = case stmts of
    [] ->
        "missing return in function "
            <> takeWhile (/= '\n') info.sourceCode
            <> "\n"
            <> "at "
            <> pack (show info.sourceLine)
            <> ":"
            <> pack (show info.sourceColumn)
    xs ->
        "missing return in function "
            <> takeWhile (/= '\n') info.sourceCode
            <> "\ngot "
            <> "\n"
            <> report (last xs)
            <> "\nexpected\n  a return statement"
errMissingRet _ = error "Front end ERROR: Should not happen"

instance Report Par.Id where
    report (Par.Id _ name) = name

instance Report Par.TopDef where
    report = \case
        Par.FnDef _ _ name _ _ -> report name
        Par.StructDef _ name _ -> report name
        Par.TypeDef _ _ name -> report name

instance Report Par.Stmt where
    report stmt =
        let info = hasInfo stmt
         in quote (takeWhile (/= '\n') info.sourceCode)
                <> " at "
                <> pack (show info.sourceLine)
                <> ":"
                <> pack (show info.sourceColumn)

instance Report Par.Arg where
    report (Par.Argument _ _ name) = report name

quote :: Text -> Text
quote s = "'" <> s <> "'"

pretty :: [Text] -> Text
pretty [] = ""
pretty (x : xs) = unlines (bold x : xs)

-- TODO: Implement
bold :: Text -> Text
bold s = s

combine :: (Report a) => a -> Text -> [Text]
combine info xs = (star `cons` ' ' `cons` xs) : [report info]

star :: Char
star = '•'

{-| Type class to help converting from the parser types
  to the type checker type
-}
class Convert a b where
    convert :: a -> b

instance (Convert a b) => Convert [a] [b] where
    convert = map convert

instance Convert Par.Type Type where
    convert = \case
        Par.Int _ -> Int
        Par.Double _ -> Double
        Par.String _ -> String
        Par.Boolean _ -> Boolean
        Par.Void _ -> Void
        Par.TVar _ t -> TVar (convert t)
        Par.Fun _ rt argtys -> Fun (convert rt) (convert argtys)
        Par.Pointer _ ty -> Pointer (convert ty)

instance Convert Par.Id Id where
    convert (Par.Id _ s) = Id s

instance Convert Par.MulOp MulOp where
    convert = \case
        Par.Times _ -> Times
        Par.Div _ -> Div
        Par.Mod _ -> Mod

instance Convert Par.AddOp AddOp where
    convert = \case
        Par.Plus _ -> Plus
        Par.Minus _ -> Minus

instance Convert Par.RelOp RelOp where
    convert = \case
        Par.LTH _ -> LTH
        Par.LE _ -> LE
        Par.GTH _ -> GTH
        Par.GE _ -> GE
        Par.EQU _ -> EQU
        Par.NE _ -> NE

instance Convert Par.Arg Arg where
    convert = \case
        Par.Argument _ typ name -> Argument (convert typ) (convert name)
