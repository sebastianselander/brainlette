{-# LANGUAGE OverloadedStrings #-}

module Frontend.Parser.TopDefParser (topdef) where

import Frontend.Parser.ArgumentParser (arg)
import Frontend.Parser.Language
    ( braces,
      id,
      identifier,
      info,
      reserved,
      reservedOp,
    )
import Frontend.Parser.ParserTypes
import Frontend.Parser.StmtParser (function)
import Text.Parsec
    ( choice,
      sepEndBy,
      try,
      (<?>),
    )
import Prelude hiding (id)

structDecl :: Parser Id
structDecl =
    uncurry IdD <$> info (reserved "struct" *> identifier)
        <?> "upper case word"

struct :: Parser TopDef
struct = do
    (i, (ident, fields)) <- info $ do
        ident <- try structDecl
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

use :: Parser TopDef
use = do
    (i, ident) <- info $ do
        ident <- reserved "use" *> id
        reservedOp ";"
        return ident
    return (Use i ident)

topdef :: Parser TopDef
topdef =
    choice
        [ use <?> "use"
        , typedef <?> "typedef"
        , struct <?> "struct"
        , FnDef NoInfo <$> function <?> "function"
        ]
