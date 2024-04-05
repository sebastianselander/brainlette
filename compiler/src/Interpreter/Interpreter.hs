{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Interpreter.Interpreter where

import BMM.Bmm
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

newtype Ctx = Ctx {functions :: Map Id (Type, [Arg], [Stmt])}

newtype Env = Env {variables :: [Map Id Value]}

data Value = IsInt Int | IsBool Bool | IsString String | IsDouble Double | Null | NoValue
    deriving (Show)

newtype RunM a = RunM {unRunM :: ReaderT Ctx (StateT Env (ExceptT Text IO)) a}
    deriving (Functor, Applicative, Monad, MonadState Env, MonadError Text, MonadReader Ctx)

interpret :: Prog -> RunM ()
interpret (Program defs) = local (\s -> s {functions = getFns defs}) interpretMain

interpretMain :: RunM ()
interpretMain = undefined

interpretFn :: [Stmt] -> Either Text Value
interpretFn stmts = undefined

interpretStmt :: Stmt -> RunM ()
interpretStmt = \case
    BStmt stmts -> withBlock (mapM_ interpretStmt stmts)
    Decl _ id -> insertVar id Null
    Ass id expr -> interpretExpr expr >>= insertVar id
    Ret mbyExpr -> undefined
    CondElse expr stmts1 stmts2 -> undefined
    Loop stmts -> undefined
    SExp expr -> undefined
    Break -> undefined

interpretExpr :: Expr -> RunM Value
interpretExpr = undefined

lookupVar :: Id -> RunM Value
lookupVar id = gets (findVar id . variables)
  where
    findVar :: Id -> [Map Id Value] -> Value
    findVar _ [] = error "type checking bug"
    findVar id (x : xs) = case Map.lookup id x of
        Nothing -> findVar id xs
        Just v -> v

insertVar :: Id -> Value -> RunM ()
insertVar id v = do
    env <- get
    let vars = env.variables
    let vars' =
            case vars of
                [] -> [Map.singleton id v]
                (x : xs) -> Map.insert id v x : xs
    put env {variables = vars'}

getFns :: [TopDef] -> Map Id (Type, [Arg], [Stmt])
getFns xs = go mempty xs
  where
    go acc [] = acc
    go acc (FnDef ty name args stmts : xs) =
        go (Map.insert name (ty, args, stmts) acc) xs

withBlock :: RunM a -> RunM a
withBlock ma =
    modify (\s -> s {variables = mempty : s.variables})
        *> ma
        <* modify (\s -> s {variables = tail s.variables})
