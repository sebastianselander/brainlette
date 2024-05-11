{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BMM.StringToTop (moveStringsToTop) where

import BMM.Bmm
import Control.Arrow ((>>>))
import Control.Monad.State (State, get, put)
import Control.Monad.State.Lazy (runState)
import Data.Text (Text)
import Utils (for, thow)

data Env = Env {counter :: Int, strings :: [Text]}

type St = State Env

name :: Int -> Text
name count = "static_string$" <> thow count

moveStringsToTop :: Prog -> Prog
moveStringsToTop =
    fixProg
        >>> flip runState (Env 0 mempty)
        >>> \(Program p, e) -> do
            Program
                ( for
                    (reverse (strings e) `zip` [0 ..])
                    (\(s, i) -> StringGlobal (name i) s)
                    <> p
                )

fixProg :: Prog -> St Prog
fixProg (Program td) = do
    td <- mapM fixTopDef td
    return $ Program td

fixTopDef :: TopDef -> St TopDef
fixTopDef = \case
    a@StringGlobal {} -> return a
    a@StructDef {} -> return a
    FnDef t i args stmts -> do
        stmts <- mapM fixStmt stmts
        return $ FnDef t i args stmts

fixStmt :: Stmt -> St Stmt
fixStmt = \case
    BStmt stmts -> BStmt <$> mapM fixStmt stmts
    Decl t i -> return $ Decl t i
    Ass ty i e -> Ass ty i <$> fixExpr e
    Ret (Just e) -> Ret . Just <$> fixExpr e
    Ret Nothing -> return $ Ret Nothing
    CondElse e s1 s2 ->
        (CondElse <$> fixExpr e)
            <*> mapM fixStmt s1
            <*> mapM fixStmt s2
    Loop expr stmts -> Loop <$> fixExpr expr <*> mapM fixStmt stmts
    SExp e -> SExp <$> fixExpr e
    Break -> return Break

fixExpr :: Expr -> St Expr
fixExpr (t, e) = case e of
    StructInit {} -> return (t, e)
    EGlobalVar {} -> return (t, e)
    ELit (LitString str) -> do
        var <- addString str
        return (t, EGlobalVar (Id var))
    i@EVar {} -> return (t, i)
    i@ELit {} -> return (t, i)
    EApp i es -> (t,) . EApp i <$> mapM fixExpr es
    Not e -> (t,) . Not <$> fixExpr e
    Neg e -> (t,) . Neg <$> fixExpr e
    EMul e1 op e2 -> do
        e1 <- fixExpr e1
        e2 <- fixExpr e2
        return (t, EMul e1 op e2)
    EAdd e1 op e2 -> do
        e1 <- fixExpr e1
        e2 <- fixExpr e2
        return (t, EAdd e1 op e2)
    ERel e1 op e2 -> do
        e1 <- fixExpr e1
        e2 <- fixExpr e2
        return (t, ERel e1 op e2)
    EAnd e1 e2 -> do
        e1 <- fixExpr e1
        e2 <- fixExpr e2
        return (t, EAnd e1 e2)
    EOr e1 e2 -> do
        e1 <- fixExpr e1
        e2 <- fixExpr e2
        return (t, EOr e1 e2)
    Cast e -> (t,) . Cast <$> fixExpr e
    Deref expr field -> (t,) <$> (Deref <$> fixExpr expr <*> return field)
    StructIndex expr field -> (t,) <$> (StructIndex <$> fixExpr expr <*> return field)
    ArrayIndex v i -> do
        v <- fixExpr v
        i <- fixExpr i
        return (t, ArrayIndex v i)
    ArrayAlloc sizes -> pure (t, ArrayAlloc sizes)
    ArrayInit exprs -> (t,) . ArrayInit <$> mapM fixExpr exprs

--- aux fucns ---
addString :: Text -> St Text
addString text = do
    state <- get
    let strs = text : strings state
    let count = counter state
    let newState = state {strings = strs, counter = count + 1}
    put newState
    return $ name count
