{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module BMM.TcToBmm (bmm) where

import BMM.Bmm
import BMM.StringToTop (moveStringsToTop)
import Control.Monad.Extra (concatMapM)
import Control.Monad.Reader (MonadReader, Reader, asks, runReader)
import Control.Monad.State (State, execState, get, put)
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Frontend.Tc.Types qualified as Tc
import Data.Set (Set)
import qualified Data.Set as Set

data TypeInfo = TI
    { typedefs :: Map Tc.Type Tc.Type
    , structs :: Map Tc.Id [Tc.Arg]
    }
    deriving (Show)

newtype Bmm a = Bmm {runBmm :: Reader TypeInfo a}
    deriving (Functor, Applicative, Monad, MonadReader TypeInfo)

bmm :: Tc.Prog -> Prog
bmm (Tc.Program defs) =
    moveStringsToTop
        . Program
        . flip
            runReader
            (execState (mapM_ extractTypedefs defs) (TI mempty mempty))
        . runBmm
        $ bmmDef defs
  where
    extractTypedefs :: Tc.TopDef -> State TypeInfo ()
    extractTypedefs = \case
        Tc.TypeDef nameLeft nameRight -> do
            ti <- get
            put $
                ti
                    { typedefs =
                        Map.insert (Tc.TVar nameRight) nameLeft ti.typedefs
                    }
        Tc.StructDef name args -> do
            ti <- get
            put $ ti {structs = Map.insert name args ti.structs}
        Tc.FnDef {} -> return ()

bmmDef :: [Tc.TopDef] -> Bmm [TopDef]
bmmDef [] = return mempty
bmmDef (x : xs) = case x of
    Tc.FnDef t id args stmts -> do
        def <-
            FnDef
                <$> bmmType t
                <*> bmmId id
                <*> mapM bmmArg args
                <*> bmmStmts stmts
        xs' <- bmmDef xs
        return (def : xs')
    Tc.TypeDef {} -> bmmDef xs
    Tc.StructDef name fields -> do
        x <-
            StructDef
                <$> bmmId name
                <*> mapM (fmap (\(Argument t _) -> t) . bmmArg) fields
        xs <- bmmDef xs
        return (x : xs)

bmmId :: Tc.Id -> Bmm Id
bmmId (Tc.Id t) = return $ Id t

data Concrete = Change | Keep

-- NOTE: Ugly, but we get stuck in a loop forever otherwise
bmmType :: Tc.Type -> Bmm Type
bmmType = go Change
  where
    go Change ty = do
        ty <- asks (fromMaybe ty . Map.lookup ty . typedefs)
        case ty of
            Tc.String -> return String
            Tc.Int -> return Int
            Tc.Double -> return Double
            Tc.Void -> return Void
            Tc.Boolean -> return Boolean
            (Tc.TVar id) -> TVar <$> bmmId id
            Tc.Fun t ts -> Fun <$> go Change t <*> mapM (go Change) ts
            Tc.Pointer ty' -> Pointer <$> go Keep ty'
            Tc.Array ty -> Array <$> go Change ty
    go Keep ty = do
        case ty of
            Tc.String -> return String
            Tc.Int -> return Int
            Tc.Double -> return Double
            Tc.Void -> return Void
            Tc.Boolean -> return Boolean
            Tc.TVar id -> TVar <$> bmmId id
            Tc.Fun t ts -> Fun <$> go Keep t <*> mapM (go Keep) ts
            Tc.Pointer ty' -> Pointer <$> go Keep ty'
            Tc.Array ty -> Array <$> go Change ty

bmmArg :: Tc.Arg -> Bmm Arg
bmmArg (Tc.Argument t id) = Argument <$> bmmType t <*> bmmId id

bmmStmts :: [Tc.Stmt] -> Bmm [Stmt]
bmmStmts s = flip concatMapM s $ \case
    Tc.BStmt stmts -> singleton . BStmt <$> bmmStmts stmts
    Tc.Decl t items -> concatMapM (itemDeclToBmm t) items
    Tc.Ass ty id expr -> singleton <$> (Ass <$> bmmType ty <*> bmmLValue id <*> bmmExpr expr)
    Tc.Incr t id -> do
        id' <- bmmId id
        t' <- bmmType t
        return
            [ Ass
                t'
                (LVar id')
                (t', EAdd (t', EVar id') Plus (t', ELit (LitInt 1)))
            ]
    Tc.Decr t id -> do
        id' <- bmmId id
        t' <- bmmType t
        return
            [ Ass
                t'
                (LVar id')
                (t', EAdd (t', EVar id') Minus (t', ELit (LitInt 1)))
            ]
    Tc.Ret e -> singleton . Ret . Just <$> bmmExpr e
    Tc.VRet -> return [Ret Nothing]
    Tc.Cond e s ->
        singleton
            <$> (CondElse <$> bmmExpr e <*> bmmStmts [s] <*> return [])
    Tc.CondElse e s1 s2 ->
        singleton
            <$> (CondElse <$> bmmExpr e <*> bmmStmts [s1] <*> bmmStmts [s2])
    Tc.While e s -> singleton <$> (Loop <$> bmmExpr e <*> bmmStmts [s]) -- Just yoink whatever is in main later
    Tc.SExp e -> do
        e' <- bmmExpr e
        return [SExp e']
    Tc.Break -> return [Break]

bmmLValue :: Tc.LValue -> Bmm LValue
bmmLValue = \case
    Tc.LVar id -> LVar <$> bmmId id
    Tc.LDeref expr@(t, _) field -> do
        fields <- fmap (fmap argName) (lookupStruct t)
        let fieldIdx =
                fromMaybe
                    (error "TC BUG: unknown field")
                    $ elemIndex field fields
        expr' <- bmmExpr expr
        return $ LDeref expr' fieldIdx

itemDeclToBmm :: Tc.Type -> Tc.Item -> Bmm [Stmt]
itemDeclToBmm t = \case
    Tc.NoInit id -> do
        id' <- bmmId id
        t' <- bmmType t
        return [Decl t' id', Ass t' (LVar id') (t', ELit $ defaultValue t')]
    Tc.Init id expr -> do
        id' <- bmmId id
        t' <- bmmType t
        expr' <- bmmExpr expr
        return [Decl t' id', Ass t' (LVar id') expr']

defaultValue :: Type -> Lit
defaultValue ty = case ty of
    TVar _ -> LitNull
    Fun _ _ -> LitNull
    Int -> LitInt 0
    String -> LitString mempty
    Boolean -> LitBool False
    Double -> LitDouble 0.0
    Void -> LitNull
    Pointer _ -> LitNull
    Array _ -> LitArrNull

bmmExpr :: Tc.Expr -> Bmm Expr
bmmExpr (ty, e) = (,) <$> bmmType ty <*> go e
  where
    go e = case e of
        Tc.EVar i -> EVar <$> bmmId i
        Tc.ELit l -> return (ELit (bmmLit l))
        Tc.EApp i e -> EApp <$> bmmId i <*> mapM bmmExpr e
        Tc.Not e -> Not <$> bmmExpr e
        Tc.EMul e1 op e2 ->
            EMul
                <$> bmmExpr e1
                <*> return (bmmMulOp op)
                <*> bmmExpr e2
        Tc.EAdd e1 op e2 ->
            EAdd
                <$> bmmExpr e1
                <*> return (bmmAddOp op)
                <*> bmmExpr e2
        Tc.ERel e1 op e2 ->
            ERel
                <$> bmmExpr e1
                <*> return (bmmRelOp op)
                <*> bmmExpr e2
        Tc.EAnd e1 e2 -> EAnd <$> bmmExpr e1 <*> bmmExpr e2
        Tc.EOr e1 e2 -> EOr <$> bmmExpr e1 <*> bmmExpr e2
        Tc.ENew _ -> do
            struct <- lookupStruct ty
            let initialize (Tc.Argument ty _) = do
                    ty' <- bmmType ty
                    let v = defaultValue ty'
                    return (ty', v)
            ENew <$> mapM initialize struct
        Tc.Neg e -> Neg <$> bmmExpr e
        Tc.Deref expr@(t, _) field -> do
            fields <- fmap (fmap argName) (lookupStruct t)
            let fieldIdx =
                    fromMaybe
                        (error "TC BUG: unknown field")
                        $ elemIndex field fields
            expr' <- bmmExpr expr
            return $ Deref expr' fieldIdx
        Tc.EIndex _ _ -> undefined
        Tc.EArray _ -> undefined

bmmLit :: Tc.Lit -> Lit
bmmLit = \case
    Tc.LitInt v -> LitInt v
    Tc.LitDouble v -> LitDouble v
    Tc.LitBool v -> LitBool v
    Tc.LitString v -> LitString v
    Tc.LitNull -> LitNull

bmmAddOp :: Tc.AddOp -> AddOp
bmmAddOp = \case
    Tc.Plus -> Plus
    Tc.Minus -> Minus

bmmMulOp :: Tc.MulOp -> MulOp
bmmMulOp = \case
    Tc.Times -> Times
    Tc.Div -> Div
    Tc.Mod -> Mod

bmmRelOp :: Tc.RelOp -> RelOp
bmmRelOp = \case
    Tc.LTH -> LTH
    Tc.LE -> LE
    Tc.GTH -> GTH
    Tc.GE -> GE
    Tc.EQU -> EQU
    Tc.NE -> NE

lookupStruct :: (MonadReader TypeInfo m) => Tc.Type -> m [Tc.Arg]
lookupStruct ty = do
    id <-
        concreteType ty >>= \case
            Tc.Pointer (Tc.TVar id) -> return id
            Tc.TVar id -> return id
            _ -> error "Bmm BUG: Lookup on a struct using a non-pointer type"
    structs <- asks structs
    case Map.lookup id structs of
        Nothing -> error $ "Bmm BUG: unknown struct '" <> show id <> "'"
        Just fields -> return fields

argName :: Tc.Arg -> Tc.Id
argName (Tc.Argument _ name) = name

concreteType :: (MonadReader TypeInfo m) => Tc.Type -> m Tc.Type
concreteType ty = do
    graph <- asks typedefs
    return $ go graph mempty ty
  where
    go :: Map Tc.Type Tc.Type -> Set Tc.Type -> Tc.Type -> Tc.Type
    go graph visited ty
        | ty `Set.member` visited = ty
        | otherwise = case Map.lookup ty graph of
            Nothing -> ty
            Just ty' -> go graph (Set.insert ty visited) ty'
