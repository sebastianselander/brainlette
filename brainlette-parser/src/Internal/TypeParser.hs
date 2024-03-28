{-# LANGUAGE OverloadedStrings #-}

module Internal.TypeParser where

import Data.Text (Text, unpack)
import Internal.Language
import ParserTypes
import Text.Parsec hiding (string)

primType :: Text -> Parser Type
primType t = (\(i,_) -> TVar i (Id i t)) <$> info (reserved (unpack t))

int :: Parser Type
int = primType "int"

double :: Parser Type
double = primType "double"

string :: Parser Type
string = primType "string"

void :: Parser Type
void = primType "void"

boolean :: Parser Type
boolean = primType "boolean"

typ :: Parser Type
typ = choice [try boolean, try int, try double, try string, void]

-- fun :: Parser Type
-- fun = do
--     traceShowM "fun"
--     (i, (ty, tys)) <- info $ do
--         ty <- typ
--         tys <- parens (commaSep typ)
--         return (ty, tys)
--     return $ Fun i ty tys
