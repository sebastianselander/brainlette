module Internal.Parser.ExprParser where

import Ast
import Internal.Parser.Language
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
id :: Parser IdSyn
id = uncurry IdSyn <$> info identifier

var :: Parser ExprSyn
var = uncurry EVarSyn <$> info id

int :: Parser ExprSyn
int = uncurry ELitIntSyn <$> info integer

double :: Parser ExprSyn
double = uncurry ELitDoubleSyn <$> info float

true :: Parser ExprSyn
true = ELitTrueSyn . fst <$> info (reserved "true")

false :: Parser ExprSyn
false = ELitFalseSyn . fst <$> info (reserved "false")

app :: Parser ExprSyn
app =
    (\(a, (b, c)) -> EAppSyn a b c)
        <$> info (((,) <$> id) <*> parens (commaSep expr))

string :: Parser ExprSyn
string = uncurry EStringSyn <$> info stringLiteral

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
            [ Prefix (NegSyn . fst <$> info (reservedOp "-"))
            , Prefix (NotSyn . fst <$> info (reservedOp "!"))
            ]
        ,
            [ Infix (mul TimesSyn . fst <$> info (reservedOp "*")) AssocLeft
            , Infix (mul DivSyn . fst <$> info (reservedOp "/")) AssocLeft
            , Infix (mul ModSyn . fst <$> info (reservedOp "%")) AssocLeft
            ]
        ,
            [ Infix (add PlusSyn . fst <$> info (reservedOp "+")) AssocLeft
            , Infix (add MinusSyn . fst <$> info (reservedOp "-")) AssocLeft
            ]
        ,
            [ Infix (rel EQUSyn . fst <$> info (reservedOp "==")) AssocNone
            , Infix (rel NESyn . fst <$> info (reservedOp "!=")) AssocNone
            , Infix (rel LESyn . fst <$> info (reservedOp "<=")) AssocNone
            , Infix (rel GESyn . fst <$> info (reservedOp ">=")) AssocNone
            , Infix (rel LTHSyn . fst <$> info (reservedOp "<")) AssocNone
            , Infix (rel GTHSyn . fst <$> info (reservedOp ">")) AssocNone
            ]
        ,
            [ Infix (EAndSyn . fst <$> info (reservedOp "&&")) AssocLeft
            ]
        ,
            [ Infix (EOrSyn . fst <$> info (reservedOp "||")) AssocLeft
            ]
        ]
      where
        add op i l = EAddSyn i l (op NoInfoSyn)
        mul op i l = EMulSyn i l (op NoInfoSyn)
        rel op i l = ERelSyn i l (op NoInfoSyn)
