{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BMM.ArrayWrap (burrito) where

import BMM.Bmm
import Control.Monad.Extra (concatMapM)
import Control.Monad.State
import Utils (pured, thow, treeMapM)

newtype WrapM a = Wrap {unWrap :: State Int a}
    deriving (Functor, Applicative, Monad, MonadState Int)

-- NOTE: Array$Internal is defined in the Runtime.hs file
burrito :: Prog -> Prog
burrito (Program defs) =
    flip evalState 0 $
        unWrap $
            Program <$> mapM wrapTopDef defs

wrapTopDef :: TopDef -> WrapM TopDef
wrapTopDef e = case e of
    FnDef rt name args stmts ->
        FnDef (wrapTy rt) name
            <$> mapM wrapArg args
            <*> concatMapM wrapStmt stmts
    StructDef name tys -> return $ StructDef name (map wrapTy tys)
    StringGlobal _ _ -> return e

freshVar :: WrapM Id
freshVar = do
    n <- get
    put (n + 1)
    return (Id $ "fresh_wrap$$_" <> thow n)

arrayName :: Id
arrayName = Id "Array$Internal"

arrayType :: Type
arrayType = TVar arrayName

wrapStmt :: Stmt -> WrapM [Stmt]
wrapStmt = \case
    BStmt stmts -> return . BStmt <$> concatMapM wrapStmt stmts
    Decl ty id -> return . return $ Decl (wrapTy ty) id
    Ass ty lv expr -> do
        pured . Ass (wrapTy ty) <$> wrapLValue lv <*> wrapExpr expr
    Ret expr -> return . Ret <$> mapM wrapExpr expr
    CondElse e trueS falseS ->
        pured $
            CondElse
                <$> wrapExpr e
                <*> concatMapM wrapStmt trueS
                <*> concatMapM wrapStmt falseS
    Loop e s -> pured . Loop <$> wrapExpr e <*> concatMapM wrapStmt s
    ArrayAlloc ty name (expr1, expr2) -> do
        arrayVar <- freshVar
        lenVar <- freshVar
        sizVar <- freshVar
        (lenE, expr1) <- wrapExpr expr1
        (sizE, expr2) <- wrapExpr expr2
        return
            [ Decl lenE lenVar
            , Ass Int (LVar lenVar) (lenE, expr1)
            , Decl sizE sizVar
            , Ass Int (LVar sizVar) (sizE, expr2)
            , ArrayAlloc (wrapTy ty) arrayVar ((lenE, EVar lenVar), (sizE, EVar sizVar))
            , Decl arrayType name
            , Ass
                arrayType
                (LVar name)
                ( arrayType
                , StructInit
                    False
                    [(Pointer Void, LitNull), (Int, LitInt 0)]
                )
            , Ass
                arrayType
                (LStructIndex (arrayType, EVar name) 0)
                (arrayType, EVar arrayVar)
            , Ass Int (LStructIndex (arrayType, EVar name) 1) (lenE, EVar lenVar)
            ]
    SExp expr -> return . SExp <$> wrapExpr expr
    Break -> return (return Break)

wrapLValue :: LValue -> WrapM LValue
wrapLValue lv = case lv of
    LIndex expr index -> do
        (ty, expr) <- wrapExpr expr
        index <- wrapExpr index
        return $ LIndex (ty, StructIndex (arrayType, expr) 0) (Int, EApp (Id "checkBound$Internal") [index, (Int, StructIndex (arrayType, expr) 1)])
    LVar id -> return $ LVar id
    LDeref e n -> LDeref <$> wrapExpr e <*> return n
    LStructIndex e n -> LStructIndex <$> wrapExpr e <*> return n

wrapExpr :: Expr -> WrapM Expr
wrapExpr (ty, e) = (wrapTy ty,) <$> go e
  where
    go :: Expr' -> WrapM Expr'
    go = treeMapM $ \case
        ArrayIndex expr index ->
            return $ ArrayIndex (Pointer Void, StructIndex expr 0) (Int, EApp (Id "checkBound$Internal") [index, (Int, StructIndex expr 1)])
        e -> return e

wrapTy :: Type -> Type
wrapTy (Array _) = arrayType
wrapTy ty = ty

wrapArg :: Arg -> WrapM Arg
wrapArg (Argument ty name) = return $ Argument (wrapTy ty) name
