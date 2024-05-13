{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BMM.ArrayWrap (burrito) where

import BMM.Bmm
import Control.Monad.Extra (concatMapM)
import Control.Monad.State
import Utils (pured, thow)

newtype WrapM a = Wrap {unWrap :: State Int a}
    deriving (Functor, Applicative, Monad, MonadState Int)

burrito :: Prog -> Prog
burrito (Program defs) = flip evalState 0 $ unWrap $ do
    Program . (arrayStruct :) <$> mapM wrapTopDef defs

wrapTopDef :: TopDef -> WrapM TopDef
wrapTopDef e = case e of
    FnDef rt name args stmts -> FnDef rt name args <$> concatMapM wrapStmt stmts
    StructDef _ _ -> return e
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

arrayStruct :: TopDef
arrayStruct = StructDef arrayName [arr, Int]
  where
    arr = Array Void

wrapStmt :: Stmt -> WrapM [Stmt]
wrapStmt = \case
    BStmt stmts -> return . BStmt <$> concatMapM wrapStmt stmts
    Decl ty id -> return (return (Decl ty id))
    Ass ty name (ety, expr) -> do
        assTy <- case ty of
            Array _ -> return arrayType
            ty -> return ty
        exprTy <- case ety of
            Array _ -> return arrayType
            ty -> return ty
        pured . Ass assTy <$> wrapLValue name <*> wrapExpr (exprTy, expr)
    Ret expr -> return . Ret <$> mapM wrapExpr expr
    CondElse e trueS falseS ->
        pured $
            CondElse
                <$> wrapExpr e
                <*> concatMapM wrapStmt trueS
                <*> concatMapM wrapStmt falseS
    Loop e s -> pured . Loop <$> wrapExpr e <*> concatMapM wrapStmt s
    ArrayAlloc ty name ((lenE, expr), (sizE, expr2)) -> do
        arrayVar <- freshVar
        lenVar <- freshVar
        sizVar <- freshVar
        return
            [ Decl lenE lenVar
            , Ass Int (LVar lenVar) (lenE, expr)
            , Decl sizE sizVar
            , Ass Int (LVar sizVar) (sizE, expr2)
            , ArrayAlloc ty arrayVar ((lenE, EVar lenVar), (sizE, EVar sizVar))
            , Decl arrayType name
            , Ass
                arrayType
                (LVar name)
                ( arrayType
                , StructInit
                    False
                    [(Array Void, LitNull), (Int, LitInt 0)]
                )
            , Ass
                (Array Void)
                (LStructIndex (arrayType, EVar name) 0)
                (Array Void, EVar arrayVar)
            , Ass Int (LStructIndex (arrayType, EVar name) 1) (lenE, EVar lenVar)
            ]
    SExp expr -> return . SExp <$> wrapExpr expr
    Break -> return (return Break)

wrapLValue :: LValue -> WrapM LValue
wrapLValue lv = case lv of
    LIndex (ty,expr) index -> return $ LIndex (ty, StructIndex (arrayType, expr) 0) index
    LVar _ -> return lv
    LDeref _ _ -> return lv
    LStructIndex _ _ -> return lv

wrapExpr :: Expr -> WrapM Expr
wrapExpr (ty, e) = (ty,) <$> go e
  where
    go :: Expr' -> WrapM Expr'
    go = \case
        EVar _ -> return e
        EGlobalVar _ -> return e
        ELit _ -> return e
        EApp name exprs -> EApp name <$> mapM wrapExpr exprs
        Not e -> Not <$> wrapExpr e
        Neg e -> Neg <$> wrapExpr e
        EMul l op r -> EMul <$> wrapExpr l <*> return op <*> wrapExpr r
        EAdd l op r -> EAdd <$> wrapExpr l <*> return op <*> wrapExpr r
        ERel l op r -> ERel <$> wrapExpr l <*> return op <*> wrapExpr r
        EAnd l r -> EAnd <$> wrapExpr l <*> wrapExpr r
        EOr l r -> EOr <$> wrapExpr l <*> wrapExpr r
        StructInit _ _ -> return e
        ArrayInit _ -> error "TODO: Not yet lifted"
        Cast expr -> Cast <$> wrapExpr expr
        Deref expr n -> Deref <$> wrapExpr expr <*> return n
        StructIndex expr n -> StructIndex <$> wrapExpr expr <*> return n
        ArrayIndex expr index ->
            return (ArrayIndex (Array Void, StructIndex expr 0) index)
