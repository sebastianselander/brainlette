{-# LANGUAGE OverloadedStrings #-}

module Frontend.Parser.TypeParser where

import Data.Text (Text, unpack)
import Data.Tuple.Extra (uncurry3)
import Frontend.Parser.Language
import Frontend.Parser.ParserTypes
import Text.Parsec hiding (upper, lower, string)
import Utils (flat3)

primType :: Text -> Parser Type
primType t = (\(i, _) -> TVar i (Id i t)) <$> info (reserved (unpack t))

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

custom :: Parser Type
custom = uncurry TVar <$> info (uncurry Id <$> info (upper <|> lower))

atom :: Parser Type
atom =
    choice
        [ try (parens typ)
        , try boolean
        , try int
        , try double
        , try string
        , try void
        , custom
        ]

funTy :: Parser Type
funTy = uncurry3 Fun . flat3 <$> info ((,) <$> atom <*> parens (commaSep typ))

typ :: Parser Type
typ = do
    ty <- choice [try funTy, atom]
    pointer <- option id (Pointer . fst <$> info (reservedOp "*"))
    return (pointer ty)
