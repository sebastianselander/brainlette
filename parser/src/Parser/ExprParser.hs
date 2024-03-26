module Parser.ExprParser where

import Ast.Types
    ( Id(Id),
      RelOp(GTH, EQU, NE, LE, GE, LTH),
      MulOp(Mod, Times, Div),
      AddOp(Minus, Plus),
      Expr(EOr, EVar, ELitInt, ELitDouble, ELitTrue, ELitFalse, EApp,
           EString, EAdd, EMul, ERel, Neg, Not, EAnd) )
import Parser.Language
    ( identifier,
      integer,
      float,
      stringLiteral,
      parens,
      reserved,
      reservedOp,
      commaSep,
      info )
import Parser.Types ( IdSyn, ExprSyn, Info(NoInfo), Parser )
import Text.Parsec ( choice, try )
import Text.Parsec.Expr
    ( buildExpressionParser,
      Assoc(AssocLeft, AssocNone),
      Operator(Infix, Prefix) )
import Prelude hiding (id, length, take)

--
id :: Parser IdSyn
id = uncurry Id <$> info identifier

var :: Parser ExprSyn
var = uncurry EVar <$> info id

int :: Parser ExprSyn
int = uncurry ELitInt <$> info integer

double :: Parser ExprSyn
double = uncurry ELitDouble <$> info float

true :: Parser ExprSyn
true = ELitTrue . fst <$> info (reserved "true")

false :: Parser ExprSyn
false = ELitFalse . fst <$> info (reserved "false")

app :: Parser ExprSyn
app =
    (\(a, (b, c)) -> EApp a b c)
        <$> info (((,) <$> id) <*> parens (commaSep expr))

string :: Parser ExprSyn
string = uncurry EString <$> info stringLiteral

atom :: Parser ExprSyn
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

expr :: Parser ExprSyn
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
