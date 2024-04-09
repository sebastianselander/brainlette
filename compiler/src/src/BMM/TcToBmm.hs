{-# LANGUAGE LambdaCase #-}

module BMM.TcToBmm (bmm) where

import BMM.Bmm
import BMM.StringToTop (moveStringsToTop)
import Data.Text (Text)
import Frontend.Tc.Types qualified as T
import Utils (for)

bmm :: T.Prog -> Either Text Prog
bmm p = pure $ toBmm p

class ToBmm a b where
    toBmm :: a -> b

instance ToBmm T.Prog Prog where
    toBmm (T.Program defs) = moveStringsToTop $ Program (map toBmm defs)

instance ToBmm T.TopDef TopDef where
    toBmm :: T.TopDef -> TopDef
    toBmm (T.FnDef t id args stmts) =
        FnDef
            (toBmm t)
            (toBmm id)
            (map toBmm args)
            (toBmm stmts)

instance ToBmm T.Id Id where
    toBmm :: T.Id -> Id
    toBmm (T.Id t) = Id t

instance ToBmm T.Type Type where
    toBmm :: T.Type -> Type
    toBmm (T.TVar id) = TVar (toBmm id)
    toBmm (T.Fun t ts) = Fun (toBmm t) (map toBmm ts)

instance ToBmm T.Arg Arg where
    toBmm :: T.Arg -> Arg
    toBmm (T.Argument t id) = Argument (toBmm t) (toBmm id)

instance ToBmm [T.Stmt] [Stmt] where
    toBmm :: [T.Stmt] -> [Stmt]
    toBmm s = concat . for s $ \case
        T.BStmt stmts -> [BStmt (toBmm stmts)]
        T.Decl t items -> concatMap (itemDeclToBmm t) items
        T.Ass id expr -> [Ass (toBmm id) (toBmm expr)]
        T.Incr t id ->
            [ Ass
                (toBmm id)
                ( toBmm t
                , EAdd
                    (toBmm t, EVar (toBmm id))
                    Plus
                    (toBmm t, ELit (LitInt 1))
                )
            ]
        T.Decr t id ->
            [ Ass
                (toBmm id)
                ( toBmm t
                , EAdd
                    (toBmm t, EVar (toBmm id))
                    Minus
                    (toBmm t, ELit (LitInt 1))
                )
            ]
        T.Ret e -> [Ret . Just . toBmm $ e]
        T.VRet -> [Ret Nothing]
        T.Cond e s -> [CondElse (toBmm e) (toBmm [s]) []]
        T.CondElse e s1 s2 -> [CondElse (toBmm e) (toBmm [s1]) (toBmm [s2])]
        T.While e s -> [Loop (CondElse (toBmm e) [Break] [] : toBmm [s])]
        T.SExp e -> [SExp (toBmm e)]
        T.Break -> [Break]

itemDeclToBmm :: T.Type -> T.Item -> [Stmt]
itemDeclToBmm t = \case
    T.NoInit id -> [Decl (toBmm t) (toBmm id)]
    T.Init id' expr -> let id = toBmm id' in [Decl (toBmm t) id, Ass id (toBmm expr)]

instance ToBmm T.Expr Expr where
    toBmm :: T.Expr -> Expr
    toBmm (t, e) = (toBmm t, toBmm e)

instance ToBmm T.Expr' Expr' where
    toBmm = \case
        T.EVar i -> EVar (toBmm i)
        T.ELit l -> ELit (toBmm l)
        T.EApp i e -> EApp (toBmm i) (map toBmm e)
        T.Neg e@(t, _) ->
            EMul
                (toBmm e)
                Times
                (toBmm t, ELit . LitInt $ (-1))
        T.Not e -> Not (toBmm e)
        T.EMul e1 op e2 -> EMul (toBmm e1) (toBmm op) (toBmm e2)
        T.EAdd e1 op e2 -> EAdd (toBmm e1) (toBmm op) (toBmm e2)
        T.ERel e1 op e2 -> ERel (toBmm e1) (toBmm op) (toBmm e2)
        T.EAnd e1 e2 -> EAnd (toBmm e1) (toBmm e2)
        T.EOr e1 e2 -> EOr (toBmm e1) (toBmm e2)

instance ToBmm T.Lit Lit where
    toBmm :: T.Lit -> Lit
    toBmm = \case
        T.LitInt v -> LitInt v
        T.LitDouble v -> LitDouble v
        T.LitBool v -> LitBool v
        T.LitString v -> LitString v

instance ToBmm T.AddOp AddOp where
    toBmm :: T.AddOp -> AddOp
    toBmm = \case
        T.Plus -> Plus
        T.Minus -> Minus

instance ToBmm T.MulOp MulOp where
    toBmm :: T.MulOp -> MulOp
    toBmm = \case
        T.Times -> Times
        T.Div -> Div
        T.Mod -> Mod

instance ToBmm T.RelOp RelOp where
    toBmm :: T.RelOp -> RelOp
    toBmm = \case
        T.LTH -> LTH
        T.LE -> LE
        T.GTH -> GTH
        T.GE -> GE
        T.EQU -> EQU
        T.NE -> NE
