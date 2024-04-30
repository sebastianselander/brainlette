{-# LANGUAGE LambdaCase #-}

module BMM.TcToBmm (bmm) where

import BMM.Bmm
import BMM.StringToTop (moveStringsToTop)
import Frontend.Tc.Types qualified as T
import Utils (for)

bmm :: T.Prog -> Prog
bmm (T.Program defs) = moveStringsToTop $ Program (map bmmDef defs)

bmmDef :: T.TopDef -> TopDef
bmmDef (T.FnDef t id args stmts) =
    FnDef
        (bmmType t)
        (bmmId id)
        (map bmmArg args)
        (bmmStmts stmts)

bmmId :: T.Id -> Id
bmmId (T.Id t) = Id t

bmmType :: T.Type -> Type
bmmType T.String = String
bmmType T.Int = Int
bmmType T.Double = Double
bmmType T.Void = Void
bmmType T.Boolean  = Boolean
bmmType (T.TVar id) = TVar (bmmId id)
bmmType (T.Fun t ts) = Fun (bmmType t) (map bmmType ts)

bmmArg :: T.Arg -> Arg
bmmArg (T.Argument t id) = Argument (bmmType t) (bmmId id)

bmmStmts :: [T.Stmt] -> [Stmt]
bmmStmts s = concat . for s $ \case
    T.BStmt stmts -> [BStmt (bmmStmts stmts)]
    T.Decl t items -> concatMap (itemDeclToBmm t) items
    T.Ass id expr -> [Ass (bmmId id) (bmmExpr expr)]
    T.Incr t id ->
        [ Ass
            (bmmId id)
            ( bmmType t
            , EAdd
                (bmmType t, EVar (bmmId id))
                Plus
                (bmmType t, ELit (LitInt 1))
            )
        ]
    T.Decr t id ->
        [ Ass
            (bmmId id)
            ( bmmType t
            , EAdd
                (bmmType t, EVar (bmmId id))
                Minus
                (bmmType t, ELit (LitInt 1))
            )
        ]
    T.Ret e -> [Ret . Just . bmmExpr $ e]
    T.VRet -> [Ret Nothing]
    T.Cond e s -> [CondElse (bmmExpr e) (bmmStmts [s]) []]
    T.CondElse e s1 s2 -> [CondElse (bmmExpr e) (bmmStmts [s1]) (bmmStmts [s2])]
    T.While e s -> [Loop (bmmExpr e) (bmmStmts [s])]
    T.SExp e -> [SExp (bmmExpr e)]
    T.Break -> [Break]

itemDeclToBmm :: T.Type -> T.Item -> [Stmt]
itemDeclToBmm t = \case
    T.NoInit id -> [Decl (bmmType t) (bmmId id)]
    T.Init id' expr -> let id = bmmId id' in [Decl (bmmType t) id, Ass id (bmmExpr expr)]

bmmExpr :: T.Expr -> Expr
bmmExpr (ty, e) = (bmmType ty, go e)
  where
    go e = case e of
        T.EVar i -> EVar (bmmId i)
        T.ELit l -> ELit (bmmLit l)
        T.EApp i e -> EApp (bmmId i) (map bmmExpr e)
        T.Neg e -> Neg (bmmExpr e)

        T.Not e -> Not (bmmExpr e)
        T.EMul e1 op e2 -> EMul (bmmExpr e1) (bmmMulOp op) (bmmExpr e2)
        T.EAdd e1 op e2 -> EAdd (bmmExpr e1) (bmmAddOp op) (bmmExpr e2)
        T.ERel e1 op e2 -> ERel (bmmExpr e1) (bmmRelOp op) (bmmExpr e2)
        T.EAnd e1 e2 -> EAnd (bmmExpr e1) (bmmExpr e2)
        T.EOr e1 e2 -> EOr (bmmExpr e1) (bmmExpr e2)

bmmLit :: T.Lit -> Lit
bmmLit = \case
    T.LitInt v -> LitInt v
    T.LitDouble v -> LitDouble v
    T.LitBool v -> LitBool v
    T.LitString v -> LitString v

bmmAddOp :: T.AddOp -> AddOp
bmmAddOp = \case
    T.Plus -> Plus
    T.Minus -> Minus

bmmMulOp :: T.MulOp -> MulOp
bmmMulOp = \case
    T.Times -> Times
    T.Div -> Div
    T.Mod -> Mod

bmmRelOp :: T.RelOp -> RelOp
bmmRelOp = \case
    T.LTH -> LTH
    T.LE -> LE
    T.GTH -> GTH
    T.GE -> GE
    T.EQU -> EQU
    T.NE -> NE
