module Internal.ProgramParser where

import Data.Text (Text)
import Internal.TopDefParser (topdef)
import ParserTypes
import Text.Parsec (ParseError, SourceName, many, parse)
import Internal.Language (whiteSpace, lexeme)

program :: SourceName -> Text -> Either ParseError Prog
program = parse (Program NoInfo <$> (whiteSpace *> many (lexeme topdef)))
