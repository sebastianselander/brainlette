{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BMM.ArrayWrap where

import BMM.Bmm

burrito :: Prog -> Prog
burrito (Program defs) = Program (arrayStruct : map wrapTopDef defs)

wrapTopDef :: TopDef -> TopDef
wrapTopDef e = case e of
    FnDef rt name args stmts -> FnDef rt name args (concatMap wrapStmt stmts)
    StructDef _ _ -> e
    StringGlobal _ _ -> e

arrayName :: Id
arrayName = Id "Array$Internal"

arrayType :: Type
arrayType = TVar arrayName

arrayStruct :: TopDef
arrayStruct = StructDef arrayName [arr, Int]
  where
    arr = Array Void

wrapStmt :: Stmt -> [Stmt]
wrapStmt = \case
    BStmt stmts -> return $ BStmt (concatMap wrapStmt stmts)
    Decl ty id -> return $ Decl ty id
    Ass ty name expr -> return $ Ass ty name (wrapExpr expr)
    Ret expr -> return $ Ret (fmap wrapExpr expr)
    CondElse e trueS falseS ->
        return $
            CondElse
                (wrapExpr e)
                (concatMap wrapStmt trueS)
                (concatMap wrapStmt falseS)
    Loop e s -> return $ Loop (wrapExpr e) (concatMap wrapStmt s)
    ArrayAlloc ty name ((lenE, expr), (sizE, expr2)) ->
        let arrayVar = Id "fresh_burrito$$0"
            lenVar = Id "fresh_burrito$$1"
            sizVar = Id "fresh_burrito$$2"
         in [ Decl lenE lenVar
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
            , Ass Int (LStructIndex (Int, EVar name) 1) (lenE, EVar lenVar)
            ]
    SExp expr -> return $ SExp (wrapExpr expr)
    Break -> return Break

wrapExpr :: Expr -> Expr
wrapExpr (ty, e) = (ty, go e)
  where
    go = \case
        EVar _ -> e
        EGlobalVar _ -> e
        ELit _ -> e
        EApp name exprs -> EApp name (map wrapExpr exprs)
        Not e -> Not (wrapExpr e)
        Neg e -> Neg (wrapExpr e)
        EMul l op r -> EMul (wrapExpr l) op (wrapExpr r)
        EAdd l op r -> EAdd (wrapExpr l) op (wrapExpr r)
        ERel l op r -> ERel (wrapExpr l) op (wrapExpr r)
        EAnd l r -> EAnd (wrapExpr l) (wrapExpr r)
        EOr l r -> EOr (wrapExpr l) (wrapExpr r)
        StructInit _ _ -> e
        ArrayInit _ -> error "TODO: Not yet lifted"
        Cast expr -> Cast (wrapExpr expr)
        Deref expr n -> Deref (wrapExpr expr) n
        StructIndex expr n -> StructIndex (wrapExpr expr) n
        ArrayIndex expr index ->
            ArrayIndex (Array Void, StructIndex expr 0) index

