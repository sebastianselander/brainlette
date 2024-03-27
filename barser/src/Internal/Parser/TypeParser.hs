{-# LANGUAGE OverloadedStrings #-}

module Internal.Parser.TypeParser where

import Ast
import Data.Text (Text, unpack)
import Internal.Parser.Language
import ParserTypes
import Text.Parsec hiding (string)

primType :: Text -> Parser TypeSyn
primType t = (\(i,_) -> TVarSyn i (IdSyn i t)) <$> info (reserved (unpack t))

int :: Parser TypeSyn
int = primType "int"

double :: Parser TypeSyn
double = primType "double"

string :: Parser TypeSyn
string = primType "string"

void :: Parser TypeSyn
void = primType "void"

typ :: Parser TypeSyn
typ = choice [try int, try double, try string, void]

-- fun :: Parser TypeSyn
-- fun = do
--     traceShowM "fun"
--     (i, (ty, tys)) <- info $ do
--         ty <- typ
--         tys <- parens (commaSep typ)
--         return (ty, tys)
--     return $ Fun i ty tys
