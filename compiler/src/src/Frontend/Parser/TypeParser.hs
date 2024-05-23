{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Parser.TypeParser where

import Control.Arrow ((>>>))
import Data.Text (Text, unpack)
import Data.Tuple.Extra (uncurry3)
import Frontend.Parser.Language
import Frontend.Parser.ParserTypes
import Text.Parsec hiding (lower, string, upper)
import Text.Parsec.Expr (Operator (Postfix), buildExpressionParser)
import Utils (flat3)

primType :: Text -> Parser Type
primType t = (\(i, _) -> TVar i (IdD i t)) <$> info (reserved (unpack t))

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

postfixTypes :: Parser Type
postfixTypes = buildExpressionParser table atom
  where
    table =
        [
            [ post $
                choice
                    [ Array . fst <$> info (reservedOp "[]")
                    , Pointer . fst <$> info (reservedOp "*")
                    ]
            ]
        ]
    post p = Postfix . chainl1 p $ return (>>>)

custom :: Parser Type
custom = uncurry TVar <$> info (uncurry IdD <$> info (upper <|> lower))

atomicType :: Parser Type
atomicType =
    choice
        [ try boolean
        , try int
        , try double
        , try string
        , try void
        , custom
        ]

atom :: Parser Type
atom =
    choice
        [ try (parens typ)
        , atomicType
        ]

funTy :: Parser Type
funTy = uncurry3 Fun . flat3 <$> info ((,) <$> atom <*> parens (commaSep typ))

typ :: Parser Type
typ = choice [try funTy, try postfixTypes, atom]
