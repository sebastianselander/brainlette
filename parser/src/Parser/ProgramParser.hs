module Parser.ProgramParser where

import Ast.Types (Prog (..))
import Data.Text (Text)
import Parser.Language (info)
import Parser.TopDefParser (topdef)
import Parser.Types (ProgSyn)
import Text.Parsec (ParseError, SourceName, many, parse)

program :: SourceName -> Text -> Either ParseError ProgSyn
program = parse (uncurry Program <$> info (many topdef))
