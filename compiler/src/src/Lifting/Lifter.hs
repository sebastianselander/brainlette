{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lifting.Lifter where

import Control.Arrow
import Control.Monad.Extra (concatMapM, mapAndUnzipM)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local, runReader, runReaderT)
import Control.Monad.State (MonadState (get, put), State, gets, modify, runState)
import Data.Data (Data)
import Data.List (nub)
import Data.List.Extra (snoc)
import Data.List.NonEmpty (toList, unzip)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Frontend.Tc.Types qualified as Tc
import Lifting.Types
import Utils (compilerPrims, listify', thow, treeMap)
import Prelude hiding (unzip)

data Env = Env
    { counter :: Int
    , liftedDefs :: [TopDef]
    , liftedDefsNames :: Set Text
    , functionRenames :: Map Id (Type, Id)
    }

newtype LiftM a = LiftM {runLift :: ReaderT (Set Tc.Id) (State Env) a}
    deriving (Functor, Applicative, Monad, MonadState Env, MonadReader (Set Tc.Id))

freshVar :: LiftM Id
freshVar = do
    n <- gets counter
    modify (\s -> s {counter = s.counter + 1})
    return (Id $ pack "lifter_fresh$$_" <> thow n)

lift :: Tc.Prog -> (Prog, Set Text)
lift prg =
    second liftedDefsNames
        . flip runState (Env 0 mempty mempty mempty)
        . flip runReaderT (toplevelNames prg)
        . runLift
        . liftProg
        $ closureWrap prg

closureWrap :: Tc.Prog -> Tc.Prog
closureWrap = treeMap fix
  where
    fix :: Tc.Type -> Tc.Type
    fix (Tc.Fun r ts) = Tc.Closure (Tc.Fun r (Tc.Pointer Tc.Void : ts))
    fix e = e

toplevelNames :: Tc.Prog -> Set Tc.Id
toplevelNames (Tc.Program defs) = Set.fromList (fmap Tc.Id compilerPrims) <> Set.fromList (listify' name defs)
  where
    name (Tc.FnDef (Tc.Fn _ name _ _)) = Just name
    name _ = Nothing

liftProg :: Tc.Prog -> LiftM Prog
liftProg (Tc.Program topdefs) = do
    defs <- mapM liftDef topdefs
    liftedDefs <- gets liftedDefs
    renames <- gets functionRenames
    return . Program . treeMap (change renames) $ liftedDefs <> defs
  where
    change :: Map Id (Type, Id) -> Expr -> Expr
    change renames (ty, EVar name) = case Map.lookup name renames of
        Nothing -> (ty, EVar name)
        Just (ty, name) -> (ty, EVar name)
    change _ e = e

liftDef :: Tc.TopDef -> LiftM TopDef
liftDef = \case
    Tc.FnDef (Tc.Fn ty "main" args stmts) ->
        FnDef True
            <$> liftTy ty
            <*> liftName "main"
            <*> mapM liftArg args
            <*> liftStmts stmts
    Tc.FnDef (Tc.Fn ty name args stmts) ->
        FnDef True
            <$> liftTy ty
            <*> liftName name
            <*> ((Argument (Pointer Void) (Id "$captures$") :) <$> mapM liftArg args)
            <*> liftStmts stmts
    Tc.StructDef name args -> StructDef <$> liftName name <*> mapM liftArg args
    Tc.TypeDef ty name -> TypeDef <$> liftTy ty <*> liftName name

declArg :: Arg -> Stmt
declArg (Argument ty id) = Decl ty [NoInit id]

declExtract :: [Arg] -> [Stmt]
declExtract = go 0
  where
    go _ [] = []
    go n (Argument ty id : xs) = ExtractFree ty n id : go (n + 1) xs

liftFn :: Tc.Function -> LiftM ([Stmt], Id)
liftFn (Tc.Fn returnType name args stmts) = do
    topNames <- ask
    let frees =
            Set.toList
                $ flip
                    runReader
                    ( Set.fromList
                        ( name
                            : fmap
                                (\(Tc.Argument _ (Tc.Id name)) -> Tc.Id name)
                                args
                        )
                        <> topNames
                    )
                $ freeVars' stmts
    frees <- mapM liftArg frees
    stmts <- liftStmts stmts
    functionVariable <- freshVar
    returnType <- liftTy returnType
    name <- liftName name
    args <- mapM liftArg args
    let (Id nm) = name
    modify $ \s -> s {liftedDefsNames = Set.insert nm s.liftedDefsNames}
    let newArgs = Argument (Pointer Void) (Id (pack "$captures$")) : args
    let funTy = Fun returnType (fmap typeOf newArgs)
    let newTy = Closure funTy
    let def =
            FnDef
                False
                returnType
                name
                newArgs
                ( LoadSelf (funTy, name) (newTy, functionVariable )
                    : map declArg frees
                    <> declExtract frees
                    <> stmts
                )
    env <- get
    put
        ( env
            { liftedDefs = def : env.liftedDefs
            , functionRenames =
                Map.insert name (newTy, functionVariable) env.functionRenames
            }
        )
    -- TODO: Change types on all usages
    return
        (
            [ Decl
                newTy
                [ Init
                    functionVariable
                    ( newTy
                    , ClosureLit (funTy, ELiftedVar name) (fmap (\(Argument ty name) -> (ty, EVar name)) frees)
                    )
                ]
            ]
        , functionVariable
        )

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
            return
                ( stmts1
                    `snoc` ForEach
                        (Argument ty name)
                        expr
                        (BStmt stmts2)
                )
        Tc.SExp e -> do
            (e, stmts) <- liftExpr e
            return (stmts `snoc` SExp e)
        Tc.Break -> return (return Break)
        Tc.SFn fn -> fst <$> liftFn fn

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
    Tc.LStructIndex l field -> do
        (l, stmts) <- liftExpr l
        field <- liftName field
        return (LStructIndex l field, stmts)

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
        Tc.EApp l exprs -> do
            (l, stmts) <- liftExpr l
            (exprs, stmts') <- liftExprs exprs
            return (EApp l exprs, stmts <> stmts')
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
        Tc.ELam args retTy expr -> do
            (Id lambdaName) <- freshVar
            (stmts, name) <- liftFn (Tc.Fn retTy (Tc.Id lambdaName) args [Tc.Ret expr])
            return (ELiftedVar name, stmts)

argToVar :: Arg -> Expr
argToVar (Argument ty name) = (ty, EVar name)

nameOf :: Arg -> Id
nameOf (Argument _ name) = name

nameOf' :: Tc.Arg -> Tc.Id
nameOf' (Tc.Argument _ name) = name

typeOf :: Arg -> Type
typeOf (Argument ty _) = ty

freeVars :: (Show a, MonadReader (Set Id) m, Data a) => [Id] -> a -> m [Arg]
freeVars names thing = do
    topNames <- ask
    let isFreeE (ty, EVar name)
            | name `Set.notMember` (Set.fromList names <> topNames) =
                Just (Argument ty name)
            | otherwise = Nothing
        isFreeE _ = Nothing
        isFreeS (Incr ty name) = Just (Argument ty name)
        isFreeS (Decr ty name) = Just (Argument ty name)
        isFreeS (Ass ty (LVar name) _) = Just (Argument ty name)
        isFreeS _ = Nothing
    return $ nub $ listify' isFreeE thing <> listify' isFreeS thing

class Free a where
    freeVars' :: (MonadReader (Set Tc.Id) m) => a -> m (Set Tc.Arg)

instance Free Tc.Expr where
    freeVars' e = freeVars' [e]

instance Free [Tc.Expr] where
    freeVars' [] = return mempty
    freeVars' ((ty, e) : xs) = case e of
        Tc.EVar name ->
            asks (Set.member name) >>= \case
                False -> Set.insert (Tc.Argument ty name) <$> freeVars' xs
                True -> freeVars' xs
        -- Lifted variables are always in scope
        Tc.ELit _ -> freeVars' xs
        Tc.EApp e es -> do
            names1 <- freeVars' e
            names2 <- freeVars' es
            ((names1 <> names2) <>) <$> freeVars' xs
        Tc.Neg e -> do
            names1 <- freeVars' e
            (names1 <>) <$> freeVars' xs
        Tc.Not e -> do
            names1 <- freeVars' e
            (names1 <>) <$> freeVars' xs
        Tc.Deref e _ -> do
            names1 <- freeVars' e
            (names1 <>) <$> freeVars' xs
        Tc.EMul e1 _ e2 -> do
            names1 <- freeVars' e1
            names2 <- freeVars' e2
            ((names1 <> names2) <>) <$> freeVars' xs
        Tc.EAdd e1 _ e2 -> do
            names1 <- freeVars' e1
            names2 <- freeVars' e2
            ((names1 <> names2) <>) <$> freeVars' xs
        Tc.ERel e1 _ e2 -> do
            names1 <- freeVars' e1
            names2 <- freeVars' e2
            ((names1 <> names2) <>) <$> freeVars' xs
        Tc.EAnd e1 e2 -> do
            names1 <- freeVars' e1
            names2 <- freeVars' e2
            ((names1 <> names2) <>) <$> freeVars' xs
        Tc.EOr e1 e2 -> do
            names1 <- freeVars' e1
            names2 <- freeVars' e2
            ((names1 <> names2) <>) <$> freeVars' xs
        Tc.StructAlloc -> freeVars' xs
        Tc.StructIndex e _ -> do
            names1 <- freeVars' e
            (names1 <>) <$> freeVars' xs
        Tc.ArrayLit es -> do
            names1 <- freeVars' es
            (names1 <>) <$> freeVars' xs
        Tc.ArrayAlloc es -> freeVars' $ toList es
        Tc.EIndex l r -> (<>) <$> freeVars' l <*> freeVars' r
        Tc.ELam args _ e -> do
            let names = fmap nameOf' args
            local (Set.fromList names <>) $ freeVars' e

instance Free Tc.Stmt where
    freeVars' s = freeVars' [s]

instance Free [Tc.Stmt] where
    freeVars' [] = return mempty
    freeVars' (s : ss) = case s of
        Tc.BStmt stmts -> do
            names <- freeVars' stmts
            names2 <- freeVars' ss
            return $ names <> names2
        Tc.Decl _ name -> local (Set.fromList (fmap itemNames name) <>) (freeVars' ss)
        Tc.Ass ty lv expr -> do
            names1 <- case lv of
                Tc.LVar name ->
                    asks (Set.member name) >>= \case
                        True -> return mempty
                        False -> return (Set.singleton (Tc.Argument ty name))
                Tc.LIndex l r -> do
                    names1 <- freeVars' l
                    names2 <- freeVars' r
                    return $ names1 <> names2
                Tc.LStructIndex l _ -> freeVars' l
                Tc.LDeref l _ -> freeVars' l
            names2 <- freeVars' expr
            ((names1 <> names2) <>) <$> freeVars' ss
        Tc.Incr ty name ->
            asks (Set.member name) >>= \case
                True -> freeVars' ss
                False -> Set.insert (Tc.Argument ty name) <$> freeVars' ss
        Tc.Decr ty name ->
            asks (Set.member name) >>= \case
                True -> freeVars' ss
                False -> Set.insert (Tc.Argument ty name) <$> freeVars' ss
        Tc.Ret expr -> do
            names1 <- freeVars' expr
            (names1 <>) <$> freeVars' ss
        Tc.VRet -> freeVars' ss
        Tc.Cond e s -> do
            names1 <- freeVars' e
            names2 <- freeVars' s
            ((names1 <> names2) <>) <$> freeVars' ss
        Tc.CondElse e s1 s2 -> do
            names1 <- freeVars' e
            names2 <- freeVars' s1
            names3 <- freeVars' s2
            ((names1 <> names2 <> names3) <>) <$> freeVars' ss
        Tc.While e s -> do
            names1 <- freeVars' e
            names2 <- freeVars' s
            ((names1 <> names2) <>) <$> freeVars' ss
        Tc.ForEach (Tc.Argument _ name) e s -> local (Set.insert name) $ do
            names1 <- freeVars' e
            names2 <- freeVars' s
            ((names1 <> names2) <>) <$> freeVars' ss
        Tc.SExp e -> do
            names <- freeVars' e
            (names <>) <$> freeVars' ss
        Tc.Break -> freeVars' ss
        Tc.SFn (Tc.Fn _ name args stmts) -> do
            let names = Set.fromList $ name : fmap nameOf' args
            names1 <- local (names <>) $ freeVars' stmts
            names2 <- freeVars' ss
            return $ names1 <> names2

itemNames :: Tc.Item -> Tc.Id
itemNames = \case
    Tc.NoInit name -> name
    Tc.Init name _ -> name

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
    Tc.Closure ty -> Closure <$> liftTy ty

liftName :: Tc.Id -> LiftM Id
liftName (Tc.Id name) = return (Id name)
