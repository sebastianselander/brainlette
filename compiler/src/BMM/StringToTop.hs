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
name count = "str$" <> thow count

moveStringsToTop :: Prog -> Prog
moveStringsToTop =
    fixProg
        >>> flip runState (Env 0 mempty)
        >>> \(Program p, e) -> do
            Program
                ( for
                    (strings e `zip` [0 ..])
                    (\(s, i) -> StringGlobal (name i) s)
                    <> p
                )

fixProg :: Prog -> St Prog
fixProg (Program td) = do
    td <- mapM fixTopDef td
    pure $ Program td

fixTopDef :: TopDef -> St TopDef
fixTopDef = \case
    a@StringGlobal {} -> pure a
    FnDef t i args stmts -> do
        stmts <- mapM fixStmt stmts
        pure $ FnDef t i args stmts

fixStmt :: Stmt -> St Stmt
fixStmt = \case
    BStmt stmts -> BStmt <$> mapM fixStmt stmts
    Decl t i -> pure $ Decl t i
    Ass i e -> Ass i <$> fixExpr e
    Ret (Just e) -> Ret . Just <$> fixExpr e
    Ret Nothing -> pure $ Ret Nothing
    CondElse e s1 s2 ->
        (CondElse <$> fixExpr e)
            <*> mapM fixStmt s1
            <*> mapM fixStmt s2
    Loop stmts -> Loop <$> mapM fixStmt stmts
    SExp e -> SExp <$> fixExpr e
    Break -> pure Break

fixExpr :: Expr -> St Expr
fixExpr (t, e) = case e of
    EString str -> do
        var <- addString str
        pure (t, EVar (Id var))
    ELit (LitString str) -> do
        var <- addString str
        pure (t, EVar (Id var))
    i@EVar {} -> pure (t, i)
    i@ELit {} -> pure (t, i)
    EApp i es -> (t,) . EApp i <$> mapM fixExpr es
    Not e -> (t,) . Not <$> fixExpr e
    EMul e1 op e2 -> do
        e1 <- fixExpr e1
        e2 <- fixExpr e2
        pure (t, EMul e1 op e2)
    EAdd e1 op e2 -> do
        e1 <- fixExpr e1
        e2 <- fixExpr e2
        pure (t, EAdd e1 op e2)
    ERel e1 op e2 -> do
        e1 <- fixExpr e1
        e2 <- fixExpr e2
        pure (t, ERel e1 op e2)
    EAnd e1 e2 -> do
        e1 <- fixExpr e1
        e2 <- fixExpr e2
        pure (t, EAnd e1 e2)
    EOr e1 e2 -> do
        e1 <- fixExpr e1
        e2 <- fixExpr e2
        pure (t, EOr e1 e2)
    Cast e -> (t,) . Cast <$> fixExpr e

--- aux fucns ---
addString :: Text -> St Text
addString text = do
    state <- get
    let strs = text : strings state
    let count = counter state
    let newState = state {strings = strs, counter = count + 1}
    put newState
    pure $ name count