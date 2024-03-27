{-# LANGUAGE OverloadedStrings #-}
module Internal.Parser.TopDefParser where

import Internal.Ast.Types (TopDef (FnDef), Id (Id))
import Internal.Parser.ArgumentParser (arg)
import Internal.Parser.ExprParser (id)
import Internal.Parser.Language (braces, commaSep, info, parens, reserved)
import Internal.Parser.StmtParser (stmt)
import Internal.Parser.TypeParser (typ)
import Internal.Parser.Types (Parser, TopDefSyn)
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
