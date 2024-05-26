module Frontend.Parser.ArgumentParser where

import Frontend.Parser.ParserTypes
import Frontend.Parser.Language
import Prelude hiding (id)
import Frontend.Parser.TypeParser (typ)

arg :: Parser Arg
arg = do
    (i, (ty, ident)) <- info $ do
        ty <- typ
        ident <- id
        return (ty, ident)
    return (Argument i ty ident)
