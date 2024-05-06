{-# LANGUAGE LambdaCase #-}

module Frontend.Parser.ExprParser where

import Frontend.Parser.Language
    ( commaSep,
      float,
      identifier,
      info,
      integer,
      parens,
      reserved,
      reservedOp,
      stringLiteral,
      upper,
    )
import Frontend.Parser.ParserTypes
import Frontend.Parser.TypeParser (typ)
import Text.Parsec (choice, optionMaybe, try)
import Text.Parsec.Expr
    ( Assoc (AssocLeft, AssocNone),
      Operator (Infix, Prefix),
      buildExpressionParser,
    )
import Prelude hiding (id, length, null, take)

--
id :: Parser Id
id = uncurry Id <$> info identifier

var :: Parser Expr
var = uncurry EVar <$> info id

int :: Parser Expr
int = uncurry ELitInt <$> info integer

double :: Parser Expr
double = uncurry ELitDouble <$> info float

true :: Parser Expr
true = ELitTrue . fst <$> info (reserved "true")

false :: Parser Expr
false = ELitFalse . fst <$> info (reserved "false")

null :: Parser Expr
null = uncurry ELitNull <$> info (optionMaybe (parens typ) <* reserved "null")

new :: Parser Expr
new = uncurry ENew <$> info (reserved "new" *> (uncurry Id <$> info upper))

app :: Parser Expr
app = do
    (info, (name, args)) <- info $ do
        name <- id
        args <- parens (commaSep expr)
        return (name, args)
    return (EApp info name args)

string :: Parser Expr
string = uncurry EString <$> info stringLiteral

atom :: Parser Expr
atom =
    choice
        [ try null
        , try double
        , try int
        , try false
        , try true
        , try string
        , try app
        , try new
        , try var
        , parens expr
        ]

expr :: Parser Expr
expr = uncurry putInfo <$> info (buildExpressionParser table atom)
  where
    table =
        [ [Infix (EDeref . fst <$> info (reservedOp "->")) AssocLeft]
        ,
            [ Prefix (Neg . fst <$> info (reservedOp "-"))
            , Prefix (Not . fst <$> info (reservedOp "!"))
            ]
        ,
            [ Infix (mul Times . fst <$> info (reservedOp "*")) AssocLeft
            , Infix (mul Div . fst <$> info (reservedOp "/")) AssocLeft
            , Infix (mul Mod . fst <$> info (reservedOp "%")) AssocLeft
            ]
        ,
            [ Infix (add Plus . fst <$> info (reservedOp "+")) AssocLeft
            , Infix (add Minus . fst <$> info (reservedOp "-")) AssocLeft
            ]
        ,
            [ Infix (rel EQU . fst <$> info (reservedOp "==")) AssocNone
            , Infix (rel NE . fst <$> info (reservedOp "!=")) AssocNone
            , Infix (rel LE . fst <$> info (reservedOp "<=")) AssocNone
            , Infix (rel GE . fst <$> info (reservedOp ">=")) AssocNone
            , Infix (rel LTH . fst <$> info (reservedOp "<")) AssocNone
            , Infix (rel GTH . fst <$> info (reservedOp ">")) AssocNone
            ]
        ,
            [ Infix (EAnd . fst <$> info (reservedOp "&&")) AssocLeft
            ]
        ,
            [ Infix (EOr . fst <$> info (reservedOp "||")) AssocLeft
            ]
        ]
      where
        add op i l = EAdd NoInfo l (op i)
        mul op i l = EMul NoInfo l (op i)
        rel op i l = ERel NoInfo l (op i)

putInfo :: SynInfo -> Expr -> Expr
putInfo i = \case
    EVar _ a -> EVar i a
    ELitInt _ a -> ELitInt i a
    ELitDouble _ a -> ELitDouble i a
    ELitTrue _ -> ELitTrue i
    ELitFalse _ -> ELitFalse i
    ELitNull _ ty -> ELitNull i ty
    EString _ a -> EString i a
    EDeref _ a b -> EDeref i a b
    EApp _ a b -> EApp i a b
    Neg _ a -> Neg i a
    Not _ a -> Not i a
    EMul _ a b c -> EMul i a b c
    EAdd _ a b c -> EAdd i a b c
    ERel _ a b c -> ERel i a b c
    EAnd _ a b -> EAnd i a b
    EOr _ a b -> EOr i a b
    ENew _ a -> ENew i a
