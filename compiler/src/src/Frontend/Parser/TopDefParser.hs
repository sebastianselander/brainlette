{-# LANGUAGE OverloadedStrings #-}
module Frontend.Parser.TopDefParser where

import Frontend.Parser.ArgumentParser (arg)
import Frontend.Parser.ExprParser (id)
import Frontend.Parser.Language (braces, commaSep, info, parens, reserved)
import Frontend.Parser.StmtParser (stmt)
import Frontend.Parser.TypeParser (typ)
import Frontend.Parser.ParserTypes
import Prelude hiding (id)
import Text.Parsec ((<|>), many)

topdef :: Parser TopDef
topdef = do
    (i, (ty, ident, args, stmts)) <- info $ do
        ty <- typ
        ident <- (flip Id "main" . fst <$> info (reserved "main")) <|> id
        args <- parens (commaSep arg)
        stmts <- braces (many stmt)
        return (ty, ident, args, stmts)
    return (FnDef i ty ident args stmts)
