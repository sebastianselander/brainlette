{-# LANGUAGE LambdaCase #-}

module Lifting.Lifter where

import Control.Arrow
import Control.Monad.Extra (concatMapM, mapAndUnzipM)
import Control.Monad.State (MonadState (get, put), State, evalState)
import Data.List.Extra (snoc)
import Data.List.NonEmpty (unzip)
import Data.Text (pack)
import Frontend.Tc.Types qualified as Tc
import Lifting.Types
import Utils (thow)
import Prelude hiding (unzip)

newtype LiftM a = LiftM {runLift :: State Int a}
    deriving (Functor, Applicative, Monad, MonadState Int)

freshVar :: LiftM Id
freshVar = do
    n <- get
    put (n + 1)
    return (Id $ pack "lifter_fresh$$_" <> thow n)

lift :: Tc.Prog -> Prog
lift = flip evalState 0 . runLift . liftProg

liftProg :: Tc.Prog -> LiftM Prog
liftProg (Tc.Program topdefs) = Program <$> mapM liftDef topdefs

liftDef :: Tc.TopDef -> LiftM TopDef
liftDef = \case
    Tc.FnDef ty name args stmts ->
        FnDef
            <$> liftTy ty
            <*> liftName name
            <*> mapM liftArg args
            <*> liftStmts stmts
    Tc.StructDef name args -> StructDef <$> liftName name <*> mapM liftArg args
    Tc.TypeDef ty name -> TypeDef <$> liftTy ty <*> liftName name

liftStmts :: [Tc.Stmt] -> LiftM [Stmt]
liftStmts = concatMapM go
  where
    go :: Tc.Stmt -> LiftM [Stmt]
    go = \case
        Tc.BStmt stmts -> return . BStmt <$> liftStmts stmts
        Tc.Decl ty items -> do
            ty <- liftTy ty
            (items, stmts) <- second concat <$> mapAndUnzipM liftItem items
            return (stmts `snoc` Decl ty items)
        Tc.Ass ty id expr -> do
            ty <- liftTy ty
            (lvalue, stmts1) <- liftLValue id
            (expr, stmts2) <- liftExpr expr
            return (stmts1 <> stmts2 `snoc` Ass ty lvalue expr)
        Tc.Incr ty id -> do
            ty <- liftTy ty
            id <- liftName id
            return (return (Incr ty id))
        Tc.Decr ty id -> do
            ty <- liftTy ty
            id <- liftName id
            return (return (Decr ty id))
        Tc.Ret e -> do
            (e, stmts) <- liftExpr e
            return (stmts `snoc` Ret e)
        Tc.VRet -> return (return VRet)
        Tc.Cond e s -> do
            (e, stmts1) <- liftExpr e
            stmts2 <- liftStmts (return s)
            return (stmts1 `snoc` Cond e (BStmt stmts2))
        Tc.CondElse e s1 s2 -> do
            (e, stmts1) <- liftExpr e
            stmts2 <- liftStmts (return s1)
            stmts3 <- liftStmts (return s2)
            return (stmts1 `snoc` CondElse e (BStmt stmts2) (BStmt stmts3))
        Tc.While e s -> do
            (e, stmts1) <- liftExpr e
            stmts2 <- liftStmts (return s)
            return (stmts1 `snoc` While e (BStmt stmts2))
        Tc.ForEach (Tc.Argument ty name) expr stmt -> do
            ty <- liftTy ty
            name <- liftName name
            (expr, stmts1) <- liftExpr expr
            stmts2 <- liftStmts (return stmt)
            return (stmts1 `snoc` ForEach (Argument ty name) expr (BStmt stmts2))
        Tc.SExp e -> do
            (e, stmts) <- liftExpr e
            return (stmts `snoc` SExp e)
        Tc.Break -> return (return Break)

liftLValue :: Tc.LValue -> LiftM (LValue, [Stmt])
liftLValue = \case
    Tc.LVar var -> do
        var <- liftName var
        return (LVar var, mempty)
    Tc.LDeref expr name -> do
        (expr, stmts) <- liftExpr expr
        name <- liftName name
        return (LDeref expr name, stmts)
    Tc.LIndex l r -> do
        (l, stmts1) <- liftExpr l
        (r, stmts2) <- liftExpr r
        return (LIndex l r, stmts1 <> stmts2)
        

liftExpr :: Tc.Expr -> LiftM (Expr, [Stmt])
liftExpr (ty, expr) = do
    ty <- liftTy ty
    (expr, stmts) <- go ty expr
    return ((ty, expr), stmts)
  where
    go :: Type -> Tc.Expr' -> LiftM (Expr', [Stmt])
    go ty = \case
        Tc.EVar id -> (,mempty) . EVar <$> liftName id
        Tc.ELit lit -> ((,mempty) . ELit <$> liftLit lit)
        Tc.EApp name exprs -> do
            name <- liftName name
            (exprs, stmts) <- liftExprs exprs
            return (EApp name exprs, stmts)
        Tc.Neg e -> first Neg <$> liftExpr e
        Tc.Not e -> first Not <$> liftExpr e
        Tc.Deref expr field -> do
            (expr, stmts) <- liftExpr expr
            field <- liftName field
            return (Deref expr field, stmts)
        Tc.StructIndex expr field -> do
            (expr, stmts) <- liftExpr expr
            field <- liftName field
            return (StructIndex expr field, stmts)
        Tc.EMul l op r -> do
            (l, lstmts) <- liftExpr l
            op <- liftMulOp op
            (r, rstmts) <- liftExpr r
            return (EMul l op r, lstmts ++ rstmts)
        Tc.EAdd l op r -> do
            (l, lstmts) <- liftExpr l
            op <- liftAddOp op
            (r, rstmts) <- liftExpr r
            return (EAdd l op r, lstmts ++ rstmts)
        Tc.ERel l op r -> do
            (l, lstmts) <- liftExpr l
            op <- liftRelOp op
            (r, rstmts) <- liftExpr r
            return (ERel l op r, lstmts ++ rstmts)
        Tc.EAnd l r -> do
            (l, lstmts) <- liftExpr l
            (r, rstmts) <- liftExpr r
            return (EAnd l r, lstmts ++ rstmts)
        Tc.EOr l r -> do
            (l, lstmts) <- liftExpr l
            (r, rstmts) <- liftExpr r
            return (EOr l r, lstmts ++ rstmts)
        Tc.EIndex l r -> do
            (l, lstmts) <- liftExpr l
            (r, rstmts) <- liftExpr r
            return (ArrayIndex l r, lstmts ++ rstmts)
        Tc.ArrayAlloc exprs -> do
            -- NOTE: The expressions must be of type `int`, thus no ArrayAlloc
            -- can exist here
            xs <- mapM (fmap fst . liftExpr) exprs
            var <- freshVar
            let stmts = return (ArrayNew ty var xs)
            return (EVar var, stmts)
        Tc.ArrayLit vals -> do
            (vals, stmts) <- liftExprs vals
            return (ArrayLit vals, stmts)
        Tc.StructAlloc -> return (StructAlloc, mempty)

liftRelOp :: Tc.RelOp -> LiftM RelOp
liftRelOp = \case
    Tc.LTH -> return LTH
    Tc.LE -> return LE
    Tc.GTH -> return GTH
    Tc.GE -> return GE
    Tc.EQU -> return EQU
    Tc.NE -> return NE

liftAddOp :: Tc.AddOp -> LiftM AddOp
liftAddOp = \case
    Tc.Plus -> return Plus
    Tc.Minus -> return Minus

liftMulOp :: Tc.MulOp -> LiftM MulOp
liftMulOp = \case
    Tc.Times -> return Times
    Tc.Div -> return Div
    Tc.Mod -> return Mod

liftExprs :: (Traversable t) => t Tc.Expr -> LiftM (t Expr, [Stmt])
liftExprs es = second concat . unzip <$> mapM liftExpr es

liftLit :: Tc.Lit -> LiftM Lit
liftLit = \case
    Tc.LitInt i -> return (LitInt i)
    Tc.LitDouble d -> return (LitDouble d)
    Tc.LitBool b -> return (LitBool b)
    Tc.LitString s -> return (LitString s)
    Tc.LitNull -> return LitNull

liftItem :: Tc.Item -> LiftM (Item, [Stmt])
liftItem = \case
    Tc.NoInit id -> do
        id <- liftName id
        return (NoInit id, mempty)
    Tc.Init id expr -> do
        id <- liftName id
        (expr, stmts) <- liftExpr expr
        return (Init id expr, stmts)

liftArg :: Tc.Arg -> LiftM Arg
liftArg = \case
    Tc.Argument ty name ->
        Argument <$> liftTy ty <*> liftName name

liftTy :: Tc.Type -> LiftM Type
liftTy = \case
    Tc.TVar name -> TVar <$> liftName name
    Tc.Fun rt tys -> Fun <$> liftTy rt <*> mapM liftTy tys
    Tc.String -> return String
    Tc.Int -> return Int
    Tc.Double -> return Double
    Tc.Boolean -> return Boolean
    Tc.Void -> return Void
    Tc.Pointer ty -> Pointer <$> liftTy ty
    Tc.Array ty -> Array <$> liftTy ty

liftName :: Tc.Id -> LiftM Id
liftName (Tc.Id name) = return (Id name)
