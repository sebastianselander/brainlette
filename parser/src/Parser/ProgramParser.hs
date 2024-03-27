module Parser.ProgramParser where

import Ast.Types (Prog (..))
import Data.Text (Text)
import Parser.TopDefParser (topdef)
import Parser.Types (ProgSyn, Info (NoInfo))
import Text.Parsec (ParseError, SourceName, many, parse)

program :: SourceName -> Text -> Either ParseError ProgSyn
program = parse (Program NoInfo <$> many topdef)
