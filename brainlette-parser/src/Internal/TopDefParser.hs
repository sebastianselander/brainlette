{-# LANGUAGE OverloadedStrings #-}
module Internal.TopDefParser where

import Internal.ArgumentParser (arg)
import Internal.ExprParser (id)
import Internal.Language (braces, commaSep, info, parens, reserved)
import Internal.StmtParser (stmt)
import Internal.TypeParser (typ)
import ParserTypes
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
