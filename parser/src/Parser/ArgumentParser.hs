module Parser.ArgumentParser where
import Parser.Types
import Ast.Types (Arg(..))
import Parser.Language
import Prelude hiding (id)
import Parser.TypeParser (typ)
import Parser.ExprParser

arg :: Parser ArgSyn
arg = do
    (i, (ty, ident)) <- info $ do
        ty <- typ
        ident <- id
        return (ty, ident)
    return (Argument i ty ident)
