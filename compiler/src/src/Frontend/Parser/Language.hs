module Frontend.Parser.Language where

import Data.Functor.Identity (Identity)
import Data.Text (Text, length, pack, take, stripEnd)
import Frontend.Parser.ParserTypes
import Text.Parsec (
    alphaNum,
    char,
    getInput,
    getPosition,
    letter,
    oneOf,
    (<|>), sourceColumn, sourceLine, sourceName, satisfy, try, (<?>), skipMany1, string, noneOf,
 )
import Text.Parsec.Language (GenLanguageDef)
import Text.Parsec.Token (GenLanguageDef (..), GenTokenParser, makeTokenParser)
import Text.Parsec.Token qualified as P
import Prelude hiding (length, take)
import Control.Monad (void)
import Text.Parsec.Prim (skipMany)
import Data.Char (isSpace)
import Data.List (nub)

brainletteDef :: GenLanguageDef Text st Identity
brainletteDef =
    LanguageDef
        { reservedOpNames =
            [ "+"
            , "++"
            , "-"
            , "--"
            , "*"
            , "%"
            , "/"
            , "!"
            , "<"
            , "<="
            , ">"
            , ">="
            , "=="
            , "!="
            , "&&"
            , "||"
            , ";"
            , ","
            , "="
            ]
        , reservedNames =
            [ "while"
            , "if"
            , "else"
            , "for"
            , "main"
            , "return"
            , "true"
            , "false"
            , "int"
            , "double"
            , "boolean"
            , "void"
            , "string"
            , "break"
            ]
        , opStart = oneOf ""
        , opLetter = oneOf ""
        , nestedComments = True
        , identStart = letter <|> char '_'
        , identLetter = alphaNum <|> char '_'
        , commentStart = "/*"
        , commentLine = "//"
        , commentEnd = "*/"
        , caseSensitive = True
        }

bl :: GenTokenParser Text st Identity
bl = makeTokenParser brainletteDef

identifier :: Parser Text
identifier = pack <$> lexeme (P.identifier bl)

integer :: Parser Integer
integer = lexeme (P.integer bl)

float :: Parser Double
float = lexeme (P.float bl)

stringLiteral :: Parser Text
stringLiteral = pack <$> lexeme (P.stringLiteral bl)

parens :: Parser a -> Parser a
parens = lexeme . P.parens bl

lexeme :: Parser a -> Parser a
lexeme = P.lexeme bl

reserved :: String -> Parser ()
reserved = lexeme . P.reserved bl

reservedOp :: String -> Parser ()
reservedOp = lexeme . P.reservedOp bl

commaSep :: Parser a -> Parser [a]
commaSep = lexeme . P.commaSep bl

commaSep1 :: Parser a -> Parser [a]
commaSep1 = lexeme . P.commaSep1 bl

semiSep :: Parser a -> Parser [a]
semiSep = lexeme . P.semiSep bl

braces :: Parser a -> Parser a
braces = lexeme . P.braces bl

whiteSpace :: Parser ()
whiteSpace = whiteSpace' brainletteDef

preProcessor :: Parser ()
preProcessor = lexeme $ void (char '#' *> skipMany (satisfy (/= '\n')))

info :: Parser a -> Parser (SynInfo, a)
info p = do
    before <- getInput
    pos <- getPosition
    let line = Text.Parsec.sourceLine pos
    let column = Text.Parsec.sourceColumn pos
    let sourceNm = Text.Parsec.sourceName pos
    a <- p
    after <- getInput
    let code = stripEnd $ take (length before - length after) before
    return (SynInfo line column (pack sourceNm) code, a)


pragma :: Parser ()
pragma = char '#' *> skipMany (satisfy (/= '\n'))

whiteSpace' :: GenLanguageDef s u m -> Parser ()
whiteSpace' languageDef
    | noLine && noMulti  = skipMany (simpleSpace <|> pragma <?> "")
    | noLine             = skipMany (simpleSpace <|> multiLineComment <|> pragma <?> "")
    | noMulti            = skipMany (simpleSpace <|> oneLineComment <|> pragma <?> "")
    | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <|> pragma <?> "")
    where
      noLine  = null (commentLine languageDef)
      noMulti = null (commentStart languageDef)


      simpleSpace =
          skipMany1 (satisfy isSpace)
  
      oneLineComment =
          do{ _ <- try (string (commentLine languageDef))
            ; skipMany (satisfy (/= '\n'))
            ; return ()
            }
  
      multiLineComment =
          do { _ <- try (string (commentStart languageDef))
             ; inComment
             }
  
      inComment
          | nestedComments languageDef  = inCommentMulti
          | otherwise                = inCommentSingle
  
      inCommentMulti
          =   do{ _ <- try (string (commentEnd languageDef)) ; return () }
          <|> do{ multiLineComment                     ; inCommentMulti }
          <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
          <|> do{ _ <- oneOf startEnd                  ; inCommentMulti }
          <?> "end of comment"
          where
            startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)
  
      inCommentSingle
          =   do{ _ <- try (string (commentEnd languageDef)); return () }
          <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
          <|> do{ _ <- oneOf startEnd                 ; inCommentSingle }
          <?> "end of comment"
          where
            startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)
  
