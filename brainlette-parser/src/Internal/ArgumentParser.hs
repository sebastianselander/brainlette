module Internal.ArgumentParser where

import ParserTypes
import Internal.Language
import Prelude hiding (id)
import Internal.TypeParser (typ)
import Internal.ExprParser

arg :: Parser Arg
arg = do
    (i, (ty, ident)) <- info $ do
        ty <- typ
        ident <- id
        return (ty, ident)
    return (Argument i ty ident)
