{-# LANGUAGE OverloadedStrings #-}

module Frontend.Parser.TypeParser where

import Data.Text (Text, unpack)
import Frontend.Parser.Language
import Frontend.Parser.ParserTypes
import Text.Parsec hiding (string)

primType :: Text -> Parser Type
primType t = (\(i,_) -> TVar i (Id i t)) <$> info (reserved (unpack t))

int :: Parser Type
int = Int . fst <$> info (reserved (unpack "int"))

double :: Parser Type
double = Double . fst <$> info (reserved (unpack "double"))

string :: Parser Type
string = String . fst <$> info (reserved (unpack "string"))

void :: Parser Type
void = Void . fst <$> info (reserved (unpack "void"))

boolean :: Parser Type
boolean = Boolean . fst <$> info (reserved (unpack "boolean"))

atom :: Parser Type
atom = choice [try (parens typ), try boolean, try int, try double, try string, void]

typ :: Parser Type
typ = choice [ try $ do
    (i, (ty, tys)) <- info $ do
        ty <- atom
        tys <- parens (commaSep typ)
        return (ty, tys)
    return $ Fun i ty tys
    , atom
    ]
