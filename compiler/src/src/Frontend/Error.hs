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
import Utils (thow)
import Prelude hiding (takeWhile, unlines, unwords)

data FEError
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
        Par.TopDef
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
    deriving (Show)

parens :: Text -> Text
parens s = "(" <> s <> ")"

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
            pretty $ combine [i|Unbound variable '#{name}'|] info
        TypeMismatch info given expected ->
            let one = case expected of
                    (x :| []) -> "'" <> report x <> "'"
                    (x :| xs) -> "one of " <> report (x : xs)
             in pretty $ combine [i|Type '#{report given}' does not match with #{one}|] info
        ExpectedFn pos typ ->
            pretty $
                combine
                    [i|Expected a function type, but got '#{report typ}'|]
                    pos
        NotComparable info op typ ->
            pretty $
                combine
                    [i|Can't perform '#{report op}' on type '#{report typ}'|]
                    info
        IllegalEmptyReturn pos typ ->
            pretty $
                combine
                    [i|Can't use empty return where a return type of '#{report typ}' is expected|]
                    pos
        ExpectedType info expected given ->
            pretty $
                combine
                    [i|Expected type '#{report expected}' but got '#{report given}'|]
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
        BreakNotInLoop info ->
            [i|break outside loop\n#{sourceCode info}\n#{sourceLine info}:#{sourceColumn info}|]
        UnreachableStatement info -> pretty $ combine [i|unreachable statement|] (oneLine info)
        MissingReturn info (Par.FnDef _ _ name _ _) -> pretty $ combine [i|missing return in function #{report name}|] (oneLine info)
        NotStatement info _ -> pretty $ combine [i|The expression is not a statement|] info
        ArgumentMismatch info name expected given -> pretty $ combine [i|in the call to the function '#{report name}', #{report expected} arguments were expected, but got #{report given} |] info
        DuplicateTopDef info tp -> pretty $ combine [i|duplicate top definition\n#{thow tp}|] info
        DuplicateArgument info tp -> pretty $ combine [i|duplicate argument in definition\n#{thow tp}|] info
        VoidDeclare info _ -> pretty $ combine [i|can not declare a variable with type '#{report Void}'|] info
        NotRelational info ty -> pretty $ combine [i|can not perform relational operations on #{report ty}|] info
        VoidParameter info ident ->
            pretty $
                combine [i|can not have parameters of type '#{report Void}' in the function #{report ident}|] info

oneLine :: SynInfo -> SynInfo
oneLine info = info { sourceCode = takeWhile (/='\n') info.sourceCode}

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

instance Report Par.Id where
    report (Par.Id _ name) = name

instance Report Par.Stmt where
    report stmt =
        let info = hasInfo stmt
         in quote (takeWhile (/= '\n') info.sourceCode)
                <> " at "
                <> pack (show info.sourceLine)
                <> ":"
                <> pack (show info.sourceColumn)

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
