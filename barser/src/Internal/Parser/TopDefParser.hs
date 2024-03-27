{-# LANGUAGE OverloadedStrings #-}
module Internal.Parser.TopDefParser where

import Ast
import Internal.Parser.ArgumentParser (arg)
import Internal.Parser.ExprParser (id)
import Internal.Parser.Language (braces, commaSep, info, parens, reserved)
import Internal.Parser.StmtParser (stmt)
import Internal.Parser.TypeParser (typ)
import ParserTypes (Parser, TopDefSyn)
import Prelude hiding (id)
import Text.Parsec ((<|>), many)

topdef :: Parser TopDefSyn
topdef = do
    (i, (ty, ident, args, stmts)) <- info $ do
        ty <- typ
        ident <- (flip IdSyn "main" . fst <$> info (reserved "main")) <|> id
        args <- parens (commaSep arg)
        stmts <- braces (many stmt)
        return (ty, ident, args, stmts)
    return (FnDefSyn i ty ident args stmts)
