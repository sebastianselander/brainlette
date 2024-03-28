module Internal.ExprParser where

import Internal.Language
    ( identifier,
      integer,
      float,
      stringLiteral,
      parens,
      reserved,
      reservedOp,
      commaSep,
      info )
import ParserTypes
import Text.Parsec ( choice, try )
import Text.Parsec.Expr
    ( buildExpressionParser,
      Assoc(AssocLeft, AssocNone),
      Operator(Infix, Prefix) )
import Prelude hiding (id, length, take)

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

app :: Parser Expr
app =
    (\(a, (b, c)) -> EApp a b c)
        <$> info (((,) <$> id) <*> parens (commaSep expr))

string :: Parser Expr
string = uncurry EString <$> info stringLiteral

atom :: Parser Expr
atom =
    choice
        [ try int
        , try double
        , try false
        , try true
        , try string
        , try app
        , var
        ]

expr :: Parser Expr
expr = buildExpressionParser table atom
  where
    table =
        [
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
        add op i l = EAdd i l (op NoInfo)
        mul op i l = EMul i l (op NoInfo)
        rel op i l = ERel i l (op NoInfo)
