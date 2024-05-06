{-# LANGUAGE OverloadedStrings #-}

module Frontend.Parser.TopDefParser where

import Frontend.Parser.ArgumentParser (arg)
import Frontend.Parser.ExprParser (id)
import Frontend.Parser.Language
    ( braces,
      commaSep,
      info,
      parens,
      reserved,
      reservedOp,
      upper,
    )
import Frontend.Parser.ParserTypes
import Frontend.Parser.StmtParser (stmt)
import Frontend.Parser.TypeParser (typ)
import Text.Parsec (choice, many, sepEndBy, try, (<|>), (<?>))
import Prelude hiding (id)

pMain :: Parser Id
pMain = flip Id "main" . fst <$> info (reserved "main")

function :: Parser TopDef
function = do
    (i, (ty, ident, args, stmts)) <- info $ do
        ty <- typ
        ident <- pMain <|> id
        args <- parens (commaSep arg)
        stmts <- braces (many stmt)
        return (ty, ident, args, stmts)
    return (FnDef i ty ident args stmts)

structDecl :: Parser Id
structDecl = uncurry Id <$> info (reserved "struct" *> upper) <?> "Expecting upper case word"

struct :: Parser TopDef
struct = do
    (i, (ident, fields)) <- info $ do
        ident <- structDecl
        fields <- braces (sepEndBy arg $ reservedOp ";")
        reservedOp ";"
        pure (ident, fields)
    return (StructDef i ident fields)

typedef :: Parser TopDef
typedef = do
    (i, (ident1, ident2)) <- info $ do
        ident <- reserved "typedef" *> structDecl
        reservedOp "*"
        t2 <- id
        reservedOp ";"
        return (ident, t2)
    return (TypeDef i ident1 ident2)

topdef :: Parser TopDef
topdef = choice [try struct, try typedef, function]
