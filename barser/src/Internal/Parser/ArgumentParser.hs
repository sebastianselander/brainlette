module Internal.Parser.ArgumentParser where

import ParserTypes
import Ast (Arg(..))
import Internal.Parser.Language
import Prelude hiding (id)
import Internal.Parser.TypeParser (typ)
import Internal.Parser.ExprParser

arg :: Parser ArgSyn
arg = do
    (i, (ty, ident)) <- info $ do
        ty <- typ
        ident <- id
        return (ty, ident)
    return (Argument i ty ident)
