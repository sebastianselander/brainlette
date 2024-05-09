module Frontend.Parser.StmtParser where

import Frontend.Parser.ExprParser
import Frontend.Parser.Language
import Frontend.Parser.ParserTypes
import Frontend.Parser.TypeParser
import Text.Parsec (try)
import Text.Parsec.Combinator (choice)
import Text.ParserCombinators.Parsec (many)
import Prelude hiding (break, id, init)
import Frontend.Parser.ArgumentParser (arg)

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
        , try foreach
        , try condElse
        , try cond
        , try blk
        , try decl <* semicolon
        , try ret <* semicolon
        , try vret <* semicolon
        , try ass <* semicolon
        , try incr <* semicolon
        , try decr <* semicolon
        , try sexp <* semicolon
        , try break <* semicolon
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
        return (ty, items)
    return $ Decl i ty items

ass :: Parser Stmt
ass = do
    (i, (lv, e)) <- info $ do
        lv <- expr
        e <- reservedOp "=" *> expr
        return (lv, e)
    return (Ass i lv e)

incr :: Parser Stmt
incr = uncurry Incr <$> info (id <* reservedOp "++")

decr :: Parser Stmt
decr = uncurry Decr <$> info (id <* reservedOp "--")

ret :: Parser Stmt
ret = uncurry Ret <$> info (reserved "return" *> expr)

vret :: Parser Stmt
vret = VRet . fst <$> info (reserved "return")

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

foreach :: Parser Stmt
foreach = do
    (i, (a, e, s)) <- info $ do
        (arg, expr) <- reserved "for" *> parens ((,) <$> arg <* reservedOp ":" <*> expr)
        s <- stmt
        return (arg, expr, s)
    return $ ForEach i a e s

sexp :: Parser Stmt
sexp = uncurry SExp <$> info expr
