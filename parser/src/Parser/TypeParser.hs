{-# LANGUAGE OverloadedStrings #-}

module Parser.TypeParser where

import Ast.Types
import Data.Text (Text, unpack)
import Parser.Language
import Parser.Types
import Text.Parsec hiding (string)

primType :: Text -> Parser TypeSyn
primType t = (\(i,_) -> TVar i (Id i t)) <$> info (reserved (unpack t))

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
