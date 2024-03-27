module Internal.Parser.ProgramParser where

import Ast
import Data.Text (Text)
import Internal.Parser.TopDefParser (topdef)
import ParserTypes
import Text.Parsec (ParseError, SourceName, many, parse)

program :: SourceName -> Text -> Either ParseError ProgSyn
program = parse (ProgramSyn NoInfoSyn <$> many topdef)
