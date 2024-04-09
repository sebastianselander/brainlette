module Frontend.Parser.ProgramParser where

import Data.Text (Text)
import Frontend.Parser.TopDefParser (topdef)
import Frontend.Parser.ParserTypes
import Text.Parsec (ParseError, SourceName, many, parse)
import Frontend.Parser.Language (whiteSpace, lexeme)

program :: SourceName -> Text -> Either ParseError Prog
program = parse (Program NoInfo <$> (whiteSpace *> many (lexeme topdef)))
