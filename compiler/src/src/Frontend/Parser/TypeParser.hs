{-# LANGUAGE OverloadedStrings #-}

module Frontend.Parser.TypeParser (typ, atomicType) where

import Control.Arrow ((>>>))
import Data.Text (unpack)
import Frontend.Parser.Language
import Frontend.Parser.ParserTypes
import Text.Parsec hiding (lower, string, upper)
import Text.Parsec.Expr (Operator (Postfix), buildExpressionParser)

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
        [ boolean
        , int
        , double
        , string
        , void
        , custom
        ]

atom :: Parser Type
atom =
    choice
        [ parens typ
        , atomicType
        ]

funTy :: Parser Type
funTy = do
    (info, (argumentTypes, returnType)) <- info $ do
        reserved "fn"
        argumentTypes <- parens (commaSep typ1)
        reservedOp "->"
        returnType <- typ
        return (argumentTypes, returnType)
    return $ Fun info returnType argumentTypes

typ1 :: Parser Type
typ1 = choice [try postfixTypes, atom]

typ :: Parser Type
typ = choice [funTy, typ1]
