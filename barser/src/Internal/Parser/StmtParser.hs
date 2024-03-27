module Internal.Parser.StmtParser where

import Ast
import Internal.Parser.ExprParser
import Internal.Parser.Language
import Internal.Parser.TypeParser
import ParserTypes
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
        return (InitSyn i ident e)

    noInit :: Parser ItemSyn
    noInit = uncurry NoInitSyn <$> info id

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
empty = EmptySyn . fst <$> info (reservedOp ";")

blk :: Parser StmtSyn
blk = uncurry BStmtSyn <$> info (braces (many stmt))

decl :: Parser StmtSyn
decl = do
    (i, (ty, items)) <- info $ do
        ty <- typ
        items <- commaSep1 item
        _ <- reservedOp ";"
        return (ty, items)
    return $ DeclSyn i ty items

ass :: Parser StmtSyn
ass = do
    (i, (ident, e)) <- info $ do
        ident <- id
        e <- reservedOp "=" *> expr <* reservedOp ";"
        return (ident, e)
    return (AssSyn i ident e)

incr :: Parser StmtSyn
incr = uncurry IncrSyn <$> info (id <* reservedOp "++" <* reservedOp ";")

decr :: Parser StmtSyn
decr = uncurry DecrSyn <$> info (id <* reservedOp "--" <* reservedOp ";")

ret :: Parser StmtSyn
ret = uncurry RetSyn <$> info (reserved "return" *> expr <* reservedOp ";")

vret :: Parser StmtSyn
vret = VRetSyn . fst <$> info (reserved "return" *> reservedOp ";")

cond :: Parser StmtSyn
cond = do
    (i, (e, s)) <- info $ do
        reserved "if"
        e <- parens expr
        s <- stmt
        return (e, s)
    return (CondSyn i e s)

condElse :: Parser StmtSyn
condElse = do
    (i, (e, s1, s2)) <- info $ do
        reserved "if"
        e <- parens expr
        s1 <- stmt
        reserved "else"
        s2 <- stmt
        return (e, s1, s2)
    return (CondElseSyn i e s1 s2)

while :: Parser StmtSyn
while = do
    (i, (e, s)) <- info $ do
        reserved "while"
        e <- parens expr
        s <- stmt
        return (e, s)
    return (WhileSyn i e s)

sexp :: Parser StmtSyn
sexp = uncurry SExpSyn <$> info (expr <* reservedOp ";")
