{-# LANGUAGE LambdaCase #-}

module Frontend.Parser.ExprParser (expr) where

import Control.Arrow ((>>>))
import Frontend.Parser.ArgumentParser (arg)
import Frontend.Parser.Language
    ( brackets,
      commaSep,
      info,
      identifier,
      namespacedIdentifier,
      parens,
      prefix,
      reserved,
      reservedOp,
      stringLiteral, naturalOrFloat
    )
import Frontend.Parser.ParserTypes
import Frontend.Parser.TypeParser (atomicType, typ)
import Text.Parsec (choice, many, many1, optionMaybe, (<?>), (<|>), option, try)
import Text.Parsec.Expr
    ( Assoc (AssocLeft, AssocNone),
      Operator (Infix, Postfix, Prefix),
      buildExpressionParser,
    )
import Prelude hiding (id, length, null, take)
import Prelude qualified (id)

id :: Parser Id
id = uncurry IdD <$> info identifier

nsId :: Parser Id
nsId = (\(i, (ns, id)) -> Id i ns id) <$> info namespacedIdentifier

var :: Parser Expr
var = uncurry EVar <$> info (try nsId <|> id)

intOrDouble :: Parser Expr
intOrDouble = do
    (f,g) <- option (Prelude.id, Prelude.id) ((negate, negate) <$ reservedOp "-")
    (info, e) <- info naturalOrFloat
    case e of
        Right double -> return . ELitDouble info $ f double
        Left int -> return . ELitInt info $ g int

true :: Parser Expr
true = ELitTrue . fst <$> info (reserved "true")

false :: Parser Expr
false = ELitFalse . fst <$> info (reserved "false")

null :: Parser Expr
null = uncurry ELitNull <$> info (optionMaybe (try $ parens typ) <* reserved "null")

new :: Parser Expr
new = do
    (i, (ident, sizes)) <- info $ do
        ident <- reserved "new" *> atomicType
        sizes <- many (brackets expr)
        return (ident, sizes)
    return $ ENew i ident sizes

app :: Parser (Expr -> Expr)
app = do
    (info, args) <- info (parens (commaSep expr))
    return $ \l -> EApp info l args

string :: Parser Expr
string = uncurry EString <$> info stringLiteral

atom :: Parser Expr
atom =
    choice
        [ null <?> "null"
        , intOrDouble <?> "number"
        , false <?> "false"
        , true <?> "true"
        , string <?> "string"
        , new <?> "new"
        , parens expr <?> "expression"
        , var <?> "variable"
        ]

index :: Parser (Expr -> Expr)
index = do
    (info, index) <- info (brackets expr) <?> "expression"
    return $ \l -> EIndex info l index

field :: Parser (Expr -> Expr)
field = do
    (info, field) <- reservedOp "." *> info id <?> "struct field"
    return $ \l -> EStructIndex info l field

deref :: Parser (Expr -> Expr)
deref = do
    (info, field) <- reservedOp "->" *> info id <?> "struct field"
    return $ \l -> EDeref info l field

lambda :: Parser (Expr -> Expr)
lambda = do
    (info, (args, ty)) <- info $ do
        _ <- reservedOp "\\"
        args <- parens (commaSep arg)
        _ <- reservedOp "->"
        returnType <- typ
        _ <- reservedOp ":"
        return (args, returnType)
    return $ \l -> ELam info args ty l

expr :: Parser Expr
expr = uncurry putInfo <$> info (buildExpressionParser table atom)
  where
    table =
        [
            [ Postfix $ foldr1 (>>>) <$> many1 (index <|> deref <|> field <|> app)
            ]
        ,
            [ prefix (Neg . fst <$> info (reservedOp "-"))
            , prefix (Not . fst <$> info (reservedOp "!"))
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
        , [ Prefix $ foldr1 (>>>) <$> many1 lambda]
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
    ENew _ a b -> ENew i a b
    EIndex _ a b -> EIndex i a b
    EStructIndex _ a b -> EStructIndex i a b
    ELam _ a b c -> ELam i a b c
