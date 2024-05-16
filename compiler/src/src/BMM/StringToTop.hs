{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BMM.StringToTop (moveStringsToTop) where

import BMM.Bmm
import Control.Arrow ((>>>))
import Control.Monad.State (State, get, put)
import Control.Monad.State.Lazy (runState)
import Data.Text (Text)
import Utils (for, thow, treeMapM)

data Env = Env {counter :: Int, strings :: [Text]}

type St = State Env

moveStringsToTop :: Prog -> Prog
moveStringsToTop =
    treeMapM fixExpr
        >>> flip runState (Env 0 mempty)
        >>> \(Program p, e) -> do
            Program
                ( for
                    (reverse (strings e) `zip` [0 ..])
                    (\(s, i) -> StringGlobal (name i) s)
                    <> p
                )

fixExpr :: Expr -> St Expr
fixExpr = \case
    (t, ELit (LitString str)) -> do
        var <- addString str
        return (t, EGlobalVar (Id var))
    e -> return e

--- aux fucns ---
addString :: Text -> St Text
addString text = do
    state <- get
    let strs = text : strings state
    let count = counter state
    let newState = state {strings = strs, counter = count + 1}
    put newState
    return $ name count

name :: Int -> Text
name count = "static_string$" <> thow count