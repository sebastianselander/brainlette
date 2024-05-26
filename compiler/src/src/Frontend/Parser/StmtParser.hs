{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Frontend.Parser.StmtParser where

import Frontend.Parser.ExprParser
import Frontend.Parser.Language
import Frontend.Parser.ParserTypes
import Frontend.Parser.TypeParser
import Text.Parsec (try, (<|>), optionMaybe)
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
        [ SFn NoInfo <$> try function
        , while
        , foreach
        , ifOptionalElse
        , blk
        , ret <* semicolon
        , break <* semicolon
        , try decl <* semicolon
        , try ass <* semicolon
        , try incr <* semicolon
        , try decr <* semicolon
        , sexp <* semicolon
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
ret = do
    (info,f) <- info $ do
        (reserved "return" *> optionMaybe expr) >>= \case
            Nothing -> return VRet
            Just e -> return $ flip Ret e
    return $ f info

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

ifOptionalElse :: Parser Stmt
ifOptionalElse = do
    (i, f) <- info $ do
        reserved "if"
        e <- parens expr
        s1 <- stmt
        res <- optionMaybe $ reserved "else" *> stmt
        case res of
            Nothing -> return $ \info -> Cond info e s1
            Just s2 -> return $ \info -> CondElse info e s1 s2
    return $ f i

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

pMain :: Parser Id
pMain = flip IdD "main" . fst <$> info (reserved "main")

function :: Parser Function
function = do
    (i, (ty, ident, args, stmts)) <- info $ do
        ty <- typ
        ident <- pMain <|> id
        args <- parens (commaSep arg)
        stmts <- braces (many stmt)
        return (ty, ident, args, stmts)
    return (Fn i ty ident args stmts)
