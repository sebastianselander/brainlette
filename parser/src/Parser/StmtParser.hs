module Parser.StmtParser where

import Ast.Types
import Parser.ExprParser
import Parser.Language
import Parser.TypeParser
import Parser.Types
import Text.Parsec (try)
import Text.Parsec.Combinator (choice)
import Prelude hiding (id, init)
import Text.ParserCombinators.Parsec (many)

-- Items

item :: Parser ItemSyn
item = choice [try init, noInit]
  where
    init :: Parser ItemSyn
    init = do
        (i, (ident, e)) <- info $ do
            ident <- id
            _ <- reservedOp "="
            e <- expr
            return (ident, e)
        return (Init i ident e)

    noInit :: Parser ItemSyn
    noInit = uncurry NoInit <$> info id

-- Statements
stmt :: Parser StmtSyn
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
        , empty
        ]

empty :: Parser StmtSyn
empty = Empty . fst <$> info (reservedOp ";")

blk :: Parser StmtSyn
blk = uncurry BStmt <$> info (braces (many stmt))

decl :: Parser StmtSyn
decl = do
    (i, (ty, items)) <- info $ do
        ty <- typ
        items <- commaSep1 item
        _ <- reservedOp ";"
        return (ty, items)
    return $ Decl i ty items

ass :: Parser StmtSyn
ass = do
    (i, (ident, e)) <- info $ do
        ident <- id
        e <- reservedOp "=" *> expr <* reservedOp ";"
        return (ident, e)
    return (Ass i ident e)

incr :: Parser StmtSyn
incr = uncurry Incr <$> info (id <* reservedOp "++" <* reservedOp ";")

decr :: Parser StmtSyn
decr = uncurry Decr <$> info (id <* reservedOp "--" <* reservedOp ";")

ret :: Parser StmtSyn
ret = uncurry Ret <$> info (reserved "return" *> expr <* reservedOp ";")

vret :: Parser StmtSyn
vret = VRet . fst <$> info (reserved "return" *> reservedOp ";")

cond :: Parser StmtSyn
cond = do
    (i, (e, s)) <- info $ do
        reserved "if"
        e <- parens expr
        s <- stmt
        return (e, s)
    return (Cond i e s)

condElse :: Parser StmtSyn
condElse = do
    (i, (e, s1, s2)) <- info $ do
        reserved "if"
        e <- parens expr
        s1 <- stmt
        reserved "else"
        s2 <- stmt
        return (e, s1, s2)
    return (CondElse i e s1 s2)

while :: Parser StmtSyn
while = do
    (i, (e, s)) <- info $ do
        reserved "while"
        e <- parens expr
        s <- stmt
        return (e, s)
    return (While i e s)

sexp :: Parser StmtSyn
sexp = uncurry SExp <$> info (expr <* reservedOp ";")
