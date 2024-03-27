module Internal.Parser.ProgramParser where

import Internal.Ast.Types (Prog (..))
import Data.Text (Text)
import Internal.Parser.TopDefParser (topdef)
import Internal.Parser.Types (ProgSyn, Info (NoInfo))
import Text.Parsec (ParseError, SourceName, many, parse)

program :: SourceName -> Text -> Either ParseError ProgSyn
program = parse (Program NoInfo <$> many topdef)
