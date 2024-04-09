module Frontend.Parser.ProgramParser where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Frontend.Parser.Language (lexeme, whiteSpace)
import Frontend.Parser.ParserTypes
import Frontend.Parser.TopDefParser (topdef)
import Text.Parsec (ParseError, SourceName, eof, many1, parse)

program :: SourceName -> Text -> Either ParseError Prog
program =
    parse (Program NoInfo <$> (whiteSpace *> (many1 (lexeme topdef) <|> [] <$ eof)))
