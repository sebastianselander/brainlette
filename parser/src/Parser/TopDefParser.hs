{-# LANGUAGE OverloadedStrings #-}
module Parser.TopDefParser where

import Ast.Types (TopDef (FnDef), Id (Id))
import Parser.ArgumentParser (arg)
import Parser.ExprParser (id)
import Parser.Language (braces, commaSep, info, parens, semiSep, reserved)
import Parser.StmtParser (stmt)
import Parser.TypeParser (typ)
import Parser.Types (Parser, TopDefSyn)
import Prelude hiding (id)
import Text.Parsec ((<|>), many)

topdef :: Parser TopDefSyn
topdef = do
    (i, (ty, ident, args, stmts)) <- info $ do
        ty <- typ
        ident <- (flip Id "main" . fst <$> info (reserved "main")) <|> id
        args <- parens (commaSep arg)
        stmts <- braces (many stmt)
        return (ty, ident, args, stmts)
    return (FnDef i ty ident args stmts)
