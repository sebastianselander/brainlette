module Internal.StmtParser where

import Internal.ExprParser
import Internal.Language
import Internal.TypeParser
import ParserTypes
import Text.Parsec (try)
import Text.Parsec.Combinator (choice)
import Prelude hiding (id, init, break)
import Text.ParserCombinators.Parsec (many)

-- Items

item :: Parser Item
item = choice [try init, noInit]
  where
    init :: Parser Item
    init = do
        (i, (ident, e)) <- info $ do
            ident <- id
            _ <- reservedOp "="
            e <- expr
            return (ident, e)
        return (Init i ident e)

    noInit :: Parser Item
    noInit = uncurry NoInit <$> info id

-- Statements
stmt :: Parser Stmt
stmt =
    choice
        [ try while
        , try condElse
        , try cond
        , try ret
        , try vret
        , try decl
        , try ass
        , try incr
        , try decr
        , try blk
        , try sexp
        , try break
        , empty
        ]

empty :: Parser Stmt
empty = Empty . fst <$> info (reservedOp ";")

break :: Parser Stmt
break = Break . fst <$> info (reserved "break")

blk :: Parser Stmt
blk = uncurry BStmt <$> info (braces (many stmt))

decl :: Parser Stmt
decl = do
    (i, (ty, items)) <- info $ do
        ty <- typ
        items <- commaSep1 item
        _ <- reservedOp ";"
        return (ty, items)
    return $ Decl i ty items

ass :: Parser Stmt
ass = do
    (i, (ident, e)) <- info $ do
        ident <- id
        e <- reservedOp "=" *> expr <* reservedOp ";"
        return (ident, e)
    return (Ass i ident e)

incr :: Parser Stmt
incr = uncurry Incr <$> info (id <* reservedOp "++" <* reservedOp ";")

decr :: Parser Stmt
decr = uncurry Decr <$> info (id <* reservedOp "--" <* reservedOp ";")

ret :: Parser Stmt
ret = uncurry Ret <$> info (reserved "return" *> expr <* reservedOp ";")

vret :: Parser Stmt
vret = VRet . fst <$> info (reserved "return" *> reservedOp ";")

cond :: Parser Stmt
cond = do
    (i, (e, s)) <- info $ do
        reserved "if"
        e <- parens expr
        s <- stmt
        return (e, s)
    return (Cond i e s)

condElse :: Parser Stmt
condElse = do
    (i, (e, s1, s2)) <- info $ do
        reserved "if"
        e <- parens expr
        s1 <- stmt
        reserved "else"
        s2 <- stmt
        return (e, s1, s2)
    return (CondElse i e s1 s2)

while :: Parser Stmt
while = do
    (i, (e, s)) <- info $ do
        reserved "while"
        e <- parens expr
        s <- stmt
        return (e, s)
    return (While i e s)

sexp :: Parser Stmt
sexp = uncurry SExp <$> info (expr <* reservedOp ";")
