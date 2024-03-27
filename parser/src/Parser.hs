module Parser (
    arg,
    expr,
    program,
    stmt,
    topdef,
    typ,
) where

import Internal.Parser.ArgumentParser (arg)
import Internal.Parser.ExprParser (expr)
import Internal.Parser.ProgramParser (program)
import Internal.Parser.StmtParser (stmt)
import Internal.Parser.TopDefParser (topdef)
import Internal.Parser.TypeParser (typ)
