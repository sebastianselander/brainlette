{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Tc.Tc (tc) where

import Control.Arrow ((>>>), second)
import Control.Monad (unless, void, when)
import Control.Monad.Extra (allM, mapMaybeM)
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, unpack, intercalate)
import Data.Tuple.Extra (uncurry3)
import Frontend.Error
import Frontend.Parser.BrainletteParser (hasInfo)
import Frontend.Parser.ParserTypes qualified as Par
import Frontend.Tc.Types qualified as Tc
import Utils (apN)
import Frontend.Parser.ParserTypes (SynInfo(NoInfo))
import Control.Monad.Writer (Writer, runWriter, MonadWriter, tell)
import Data.DList ( DList, toList, singleton )

tc :: Par.Prog -> Either Text Tc.Prog
tc p =
    let (env, ctx) = addDefs p
        (prg, errs) = tcProg >>> runTcM >>> flip evalStateT env >>> flip runReaderT ctx >>> runWriter >>> second toList$ p
    in case errs of
        [] -> Right prg
        xs -> Left $ intercalate "\n\n" $ fmap report xs


data Env = Env
    { variables :: Map Tc.Id Tc.Type
    , functions :: Map Tc.Id Tc.Type
    }
    deriving (Show, Eq, Ord)

data Ctx = Ctx
    { defStack :: [Par.Function]
    , exprStack :: [Par.Expr]
    , subtypes :: Map Tc.Type [Tc.Type]
    , typedefGraph :: Map Tc.Type Tc.Type
    , structs :: Map Tc.Type (Map Tc.Id Tc.Type)
    }
    deriving (Show, Eq, Ord)

newtype TcM a = TC {runTcM :: StateT Env (ReaderT Ctx (Writer (DList FEError))) a}
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader Ctx
        , MonadState Env
        , MonadWriter (DList FEError)
        )


tcProg :: Par.Prog -> TcM Tc.Prog
tcProg (Par.Program _ defs) = Tc.Program <$> mapM infDef defs

infDef :: Par.TopDef -> TcM Tc.TopDef
infDef = \case
    Par.TypeDef _ name1 name2 ->
        return $
            Tc.TypeDef
                (Tc.Pointer $ Tc.TVar $ convert name1)
                (convert name2)
    Par.StructDef _ name args -> do
        let checkVoid (Par.Argument info ty _) = case ty of
                Par.Void _ -> throw (VoidField info)
                _ -> return ()
        mapM_ checkVoid args
        return $ Tc.StructDef (convert name) (convert args)
    Par.FnDef _ fn -> Tc.FnDef <$> infFn fn
    Par.Use _ _ -> error "Typechecker: use should not exist"

infFn :: Par.Function -> TcM Tc.Function
infFn self@(Par.Fn info returnType name args block) = do
        rt' <- typeExist info returnType
        let name' = convert name
        let fnType = Tc.Fun rt' (map typeOf args)
        insertFunc name' fnType
        block' <- pushDef self $ do
            mapM_ (insertArg name) args
            mapMaybeM (tcStmt rt') block
        return (Tc.Fn rt' name' (convert args) block')

tcStmt :: Tc.Type -> Par.Stmt -> TcM (Maybe Tc.Stmt)
tcStmt retTy = go
  where
    go :: Par.Stmt -> TcM (Maybe Tc.Stmt)
    go s = case s of
        Par.Empty _ -> return (fail "Removing empty blocks")
        Par.BStmt _ stmts -> Just . Tc.BStmt <$> mapMaybeM go stmts
        Par.Decl info typ items -> do
            typ' <- typeExist info typ
            when (isVoid typ') (throw $ VoidDeclare info s)
            Just . Tc.Decl typ' <$> mapM (tcItem info typ') items
          where
            tcItem :: Par.SynInfo -> Tc.Type -> Par.Item -> TcM Tc.Item
            tcItem info expected = \case
                Par.NoInit _ name -> do
                    let name' = convert name
                    exist <- doesVariableExist name'
                    when exist $ throw (BoundVariable info name')
                    insertVar name' expected
                    return $ Tc.NoInit name'
                Par.Init _ name expr -> do
                    let name' = convert name
                    -- TODO: move this check to renamer
                    exist <- doesVariableExist name'
                    when exist $ throw (BoundVariable info name')
                    (ty, expr') <- infExpr expr
                    ty' <- unify info expected ty
                    insertVar name' ty'
                    return $ Tc.Init name' (ty', expr')
        Par.Ass info lhs rhs -> do
            case lhs of
                Par.EVar _ ident -> do
                    -- TODO: Catch unbound *variables* in the renamer,
                    -- currently it treats functions and variables the same
                    mbyTy <- lookupVar ident
                    identTy <-
                        maybe
                            (throw $ UnboundVariable info (convert ident))
                            return
                            mbyTy
                    (ty, rhs') <- infExpr rhs
                    ty' <- unify info identTy ty
                    return . return $ Tc.Ass ty (Tc.LVar (convert ident)) (ty', rhs')
                Par.EDeref {} -> do
                    (tyl, lhs') <- infExpr lhs
                    case lhs' of
                        Tc.Deref e id -> do
                            (tyr, rhs') <- infExpr rhs
                            ty <- unify info tyl tyr
                            return . return $ Tc.Ass ty (Tc.LDeref e id) (ty, rhs')
                        _ -> error "TYPECHECK BUG: Dereference"
                Par.EIndex _ l r -> do
                    l' <- infExpr l
                    case typeOf l' of
                        Tc.Array ty -> do
                            r'@(tyr, _) <- infExpr r
                            unless (isInt tyr) (throw $ ExpectedType info Tc.Int tyr)
                            (tyr, rhs') <- infExpr rhs
                            ty <- unify info ty tyr
                            return . return $ Tc.Ass ty (Tc.LIndex l' r') (ty, rhs')
                        ty -> throw $ ExpectedArray info ty
                Par.EStructIndex _ l field -> do
                    l@(tyl, _) <- infExpr l
                    fields <- getStruct info tyl
                    let field' = convert field
                    case Map.lookup field' fields of
                        Just ty -> do
                            (tyr, rhs) <- infExpr rhs
                            ty <- unify info ty tyr
                            return . return $ Tc.Ass ty (Tc.LStructIndex l field') (ty, rhs)
                        Nothing -> throw $ UnboundField info field
                _ -> throw $ NotLValue info lhs
        Par.Incr info var -> do
            typ <- getVar var
            unless (isInt typ) (throw $ ExpectedType info Tc.Int typ)
            errNotNumber info typ
            return (Just (Tc.Incr typ (convert var)))
        Par.Decr info var -> do
            typ <- getVar var
            errNotNumber info typ
            return (Just (Tc.Decr typ (convert var)))
        Par.Ret info expr -> do
            (ty, expr') <- infExpr expr
            ty' <- unify info retTy ty
            return (Just (Tc.Ret (ty', expr')))
        Par.VRet pos -> do
            unless (isVoid retTy) $ throw (IllegalEmptyReturn pos retTy)
            return (Just Tc.VRet)
        Par.Cond _ cond stmt -> do
            cond' <- infExpr cond
            errNotBoolean (hasInfo cond) (typeOf cond')
            stmt' <- go stmt
            let statement = fromMaybe (Tc.BStmt []) stmt'
            return $ Just (Tc.Cond cond' statement)
        Par.CondElse _ cond stmt1 stmt2 -> do
            cond' <- infExpr cond
            errNotBoolean (hasInfo cond) (typeOf cond')
            stmt1' <- fromMaybe (Tc.BStmt []) <$> go stmt1
            stmt2' <- fromMaybe (Tc.BStmt []) <$> go stmt2
            return (Just (Tc.CondElse cond' stmt1' stmt2'))
        Par.While _ cond stmt -> do
            cond' <- infExpr cond
            errNotBoolean (hasInfo cond) (typeOf cond')
            stmt' <- fromMaybe (Tc.BStmt []) <$> go stmt
            return (Just (Tc.While cond' stmt'))
        Par.ForEach _ (Par.Argument info ty name) expr stmt -> do
            ty' <- typeExist info ty
            let name' = convert name
            expr' <- tcExpr (Tc.Array ty') expr
            insertVar name' ty'
            stmt' <- fromMaybe (Tc.BStmt []) <$> go stmt
            return . Just $ Tc.ForEach (Tc.Argument ty' name') expr' stmt'
        Par.SExp _ expr@(Par.EApp {}) -> Just . Tc.SExp <$> infExpr expr
        Par.SExp info expr -> throw (NotStatement info expr)
        Par.Break _ -> return $ Just Tc.Break
        Par.SFn _ fn -> Just . Tc.SFn <$> infFn fn
        Par.ForI _ s1 e s2 body -> do
            s1 <- fromMaybe (Tc.BStmt []) <$> go s1
            e <- tcExpr Tc.Boolean e
            s2 <- fromMaybe (Tc.BStmt []) <$> go s2
            body <- fromMaybe (Tc.BStmt []) <$> go body
            return . Just $ Tc.ForI s1 e s2 body

typeExist :: Par.SynInfo -> Par.Type -> TcM Tc.Type
typeExist info ty = do
    let ty' = convert ty
    doesExist <- go ty'
    unless doesExist (throw (UnboundType info ty'))
    return ty'
  where
    go :: Tc.Type -> TcM Bool
    go = \case
        ty@Tc.TVar {} -> do
            isStruct <- asks (Map.member ty . structs)
            isTypeDef <- asks (Map.member ty . typedefGraph)
            return $ isStruct || isTypeDef
        Tc.Fun rt argTys -> (&&) <$> go rt <*> allM go argTys
        Tc.String -> return True
        Tc.Int -> return True
        Tc.Double -> return True
        Tc.Boolean -> return True
        Tc.Void -> return True
        Tc.Pointer ty -> go ty
        Tc.Array ty -> go ty
        Tc.Closure ty -> go ty

tcExpr :: Tc.Type -> Par.Expr -> TcM Tc.Expr
tcExpr ty e = do
    e' <- infExpr e
    void (unify (hasInfo e) ty (typeOf e'))
    return e'

infExpr :: Par.Expr -> TcM Tc.Expr
infExpr e = pushExpr e $ case e of
    Par.ELitInt _ n -> return (Tc.Int, Tc.ELit (Tc.LitInt n))
    Par.ELitDouble _ n -> return (Tc.Double, Tc.ELit (Tc.LitDouble n))
    Par.ELitTrue _ -> return (Tc.Boolean, Tc.ELit (Tc.LitBool True))
    Par.ELitFalse _ -> return (Tc.Boolean, Tc.ELit (Tc.LitBool False))
    Par.ELitNull info ty ->
        (,Tc.ELit Tc.LitNull)
            <$> maybe (throw $ TypeUninferrable info e) (return . convert) ty
    Par.EString _ str -> return (Tc.String, Tc.ELit (Tc.LitString str))
    Par.ENew info ty sizes -> do
        tyC <- typeExist info ty
        tyC' <- concreteType tyC
        case ty of
            Par.TVar {} -> do
                case tyC' of
                    Tc.Pointer ty -> void $ getStruct info ty
                    ty -> void $ getStruct info ty
            Par.Array {} -> return ()
            _
                | null sizes -> throw $ NotNewable info tyC
                | otherwise -> return ()
        case sizes of
            [] -> do
                asks (Map.lookup tyC . typedefGraph) >>= \case
                    Nothing -> return (Tc.Pointer tyC, Tc.StructAlloc)
                    Just ty -> return (ty, Tc.StructAlloc)
            (sz : szs) -> do
                sz <- tcExpr Tc.Int sz
                szs <- mapM (tcExpr Tc.Int) szs
                let ty' = apN (length sizes) Tc.Array tyC
                return (ty', Tc.ArrayAlloc (sz :| szs))
    Par.EDeref info l field -> do
        l' <- infExpr l
        ty <-
            concreteType (typeOf l') >>= \case
                Tc.Pointer ty -> return ty
                ty -> throw $ NotPointer info ty
        fields <- getStruct info ty
        let id' = convert field
        ty <-
            maybe
                (throw $ UnboundField info field)
                return
                (Map.lookup id' fields)
        return (ty, Tc.Deref l' id')
    Par.EVar _ i -> do
        ty <- getVar i
        return (ty, Tc.EVar (convert i))
    Par.Neg info expr -> do
        expr' <- infExpr expr
        let ty = typeOf expr'
        errNotNumber info ty
        return (ty, Tc.Neg expr')
    Par.Not info expr -> do
        expr' <- infExpr expr
        let ty = typeOf expr'
        errNotBoolean info ty
        return (ty, Tc.Not expr')
    Par.EMul info l op r -> do
        l' <- infExpr l
        errNotNumber info (typeOf l')
        r' <- infExpr r
        ty <- unify info (typeOf l') (typeOf r')
        return (ty, Tc.EMul l' (convert op) r')
    Par.EAdd info l op r -> do
        l' <- infExpr l
        errNotNumber info (typeOf l')
        r' <- infExpr r
        ty <- unify info (typeOf l') (typeOf r')
        return (ty, Tc.EAdd l' (convert op) r')
    Par.EAnd info l r -> do
        l' <- infExpr l
        errNotBoolean info (typeOf l')
        r' <- infExpr r
        errNotBoolean info (typeOf r')
        return (Tc.Boolean, Tc.EAnd l' r')
    Par.EOr info l r -> do
        l' <- infExpr l
        errNotBoolean info (typeOf l')
        r' <- infExpr r
        errNotBoolean info (typeOf r')
        return (Tc.Boolean, Tc.EOr l' r')
    Par.ERel info l op r -> do
        (tl, l') <- infExpr l
        (tr, r') <- infExpr r
        ty <- case (tl, tr) of
            (Tc.Double, Tc.Int) -> return Tc.Double
            (Tc.Int, Tc.Double) -> return Tc.Double
            (Tc.Int, Tc.Int) -> return Tc.Int
            (Tc.Double, Tc.Double) -> return Tc.Double
            (Tc.Boolean, Tc.Boolean) -> return Tc.Boolean
            (Tc.String, Tc.String) -> throw (ErrText info "String comparison not yet supported")
            (l, r) -> do
                ty1 <- concreteType l
                ty2 <- concreteType r
                if
                    | ty1 == ty2 && isPointer ty1 -> return ty1
                    | ty1 == ty2 && not (isPointer ty1) -> throw (NotRelational info l)
                    | otherwise -> throw (TypeMismatch info ty1 [ty2])
        return (Tc.Boolean, Tc.ERel (ty, l') (convert op) (ty, r'))
    Par.EApp p l exprs -> do
        (lty, l') <- infExpr l
        case lty of
            Tc.Fun rt argtys -> do
                let infos = map hasInfo exprs
                exprs' <- mapM infExpr exprs
                let expecteds = length argtys
                    givens = length exprs'
                unless (expecteds == givens) $
                    throw (ArgumentMismatch p l expecteds givens)
                let infoTys = zip3 infos argtys (map typeOf exprs')
                mapM_ (uncurry3 unify) infoTys
                return (rt, Tc.EApp (lty, l') exprs')
            _else -> throw (ExpectedFn p lty)
    Par.EIndex info l r -> do
        l' <- infExpr l
        case typeOf l' of
            Tc.Array ty -> do
                r'@(tyr, _) <- infExpr r
                unless (isInt tyr) (throw $ ExpectedType info Tc.Int tyr)
                return (ty, Tc.EIndex l' r')
            ty -> throw $ ExpectedArray info ty
    Par.EStructIndex info l field -> do
        expr <- infExpr l
        case typeOf expr of
            Tc.Array _ -> do
                -- NOTE: For now hard coded for arrays
                if
                    | (Par.IdD _ "length") <- field -> return (Tc.Int, Tc.StructIndex expr (Tc.Id "length"))
                    | otherwise -> throw $ UnboundField info field
            ty -> do
                fieldTy <- Map.lookup (convert field) <$> getStruct info ty
                ty' <- case fieldTy of
                    Nothing -> throw $ UnboundField info field
                    Just ty -> return ty
                return (ty', Tc.StructIndex expr (convert field))
    Par.ELam info args ty expr -> do
        args <- mapM (insertArg (Par.IdD NoInfo "lambda")) args
        (exprTy, expr) <- infExpr expr
        ty <- unify info (convert ty) exprTy
        return (Tc.Fun ty (fmap typeOf args), Tc.ELam args ty (ty, expr))


isPointer :: Tc.Type -> Bool
isPointer (Tc.Pointer _) = True
isPointer _ = False

{-| Extract the necessary information from all top level definitions before
continuing
-}
addDefs :: Par.Prog -> (Env, Ctx)
addDefs (Par.Program _ defs) =
    let prelude =
            [ (Tc.Id "printInt", Tc.Fun Tc.Void [Tc.Int])
            , (Tc.Id "printDouble", Tc.Fun Tc.Void [Tc.Double])
            , (Tc.Id "printString", Tc.Fun Tc.Void [Tc.String])
            , (Tc.Id "readInt", Tc.Fun Tc.Int [])
            , (Tc.Id "readDouble", Tc.Fun Tc.Double [])
            , (Tc.Id "readString", Tc.Fun Tc.String [])
            ]
     in foldr @[]
            getDefs
            ( Env mempty prelude
            , Ctx mempty mempty mempty mempty mempty
            )
            defs
  where
    getDefs def (env, ctx) = case def of
        Par.TypeDef _ name1 name2 -> do
            let name1' = convert name1
                name2' = convert name2
            ( env
                , ctx
                    { typedefGraph =
                        Map.insert
                            (Tc.TVar name2')
                            (Tc.Pointer (Tc.TVar name1'))
                            ctx.typedefGraph
                    }
                )
        Par.StructDef _ name args -> do
            let tuple (Par.Argument _ ty ident) = (convert ident, convert ty)
                fields = foldr (uncurry Map.insert . tuple) mempty args
            ( env
                , ctx
                    { structs =
                        Map.insert (Tc.TVar $ convert name) fields ctx.structs
                    }
                )
        Par.FnDef _ (Par.Fn _ ty name args _) ->
            ( env
                { functions =
                    Map.insert
                        (convert name)
                        (Tc.Fun (convert ty) (map typeOf args))
                        env.functions
                }
            , ctx
            )
        Par.Use _ _ -> error "Typechecker: use should not exist"

lookupVar :: Par.Id -> TcM (Maybe Tc.Type)
lookupVar i =
    gets (Map.lookup (convert i) . variables) >>= \case
        Nothing -> return Nothing
        Just rt -> return (return rt)

getStruct :: Par.SynInfo -> Tc.Type -> TcM (Map Tc.Id Tc.Type)
getStruct info ty@(Tc.TVar ty') = do
    mby <- asks (Map.lookup ty . structs)
    maybe (throw (UnboundStruct info ty')) return mby
getStruct info ty = throw (NotStructType info ty)

getVar :: Par.Id -> TcM Tc.Type
getVar i@(Par.Id _ _ _i) = do
    mbTy <- gets (Map.lookup (convert i) . variables)
    case mbTy of
        Nothing ->
            gets
                ( fromMaybe
                    ( error $
                        "TYPECHECKER: Variable lookup bug. \
                        \ Please report\nCould not find name: "
                            <> unpack _i
                            <> "\n"
                    )
                    . Map.lookup (convert i)
                    . functions
                )
        Just ty -> return ty

doesVariableExist :: Tc.Id -> TcM Bool
doesVariableExist name = gets (Map.member name . variables)

insertArg :: Par.Id -> Par.Arg -> TcM Tc.Arg
insertArg ident (Par.Argument info (Par.Void _) _) =
    throw $ VoidParameter info ident
insertArg _ (Par.Argument info ty name) = do
    ty <- typeExist info ty
    let name' = convert name
    insertVar name' ty
    return (Tc.Argument ty name')

insertVar :: Tc.Id -> Tc.Type -> TcM ()
insertVar name typ =
    modify (\s -> s {variables = Map.insert name typ s.variables})

insertFunc :: Tc.Id -> Tc.Type -> TcM ()
insertFunc name typ =
    modify (\s -> s {functions = Map.insert name typ s.functions})

errNotBoolean :: (MonadWriter (DList FEError) m) => Par.SynInfo -> Tc.Type -> m ()
errNotBoolean info = \case
    Tc.Boolean -> return ()
    other -> throw (ExpectedType info Tc.Boolean other)

errNotNumber :: (MonadWriter (DList FEError) m) => Par.SynInfo -> Tc.Type -> m ()
errNotNumber info ty
    | isNumber ty = return ()
    | otherwise = throw (ExpectedNumber info ty)

isVoid :: Tc.Type -> Bool
isVoid Tc.Void = True
isVoid _ = False

isNumber :: Tc.Type -> Bool
isNumber Tc.Int = True
isNumber Tc.Double = True
isNumber _ = False

isInt :: Tc.Type -> Bool
isInt Tc.Int = True
isInt _ = False

unify ::
    -- | Parsing information
    Par.SynInfo ->
    -- | Expected type
    Tc.Type ->
    -- | Given type
    Tc.Type ->
    -- | The type to use considering automatic type casting
    TcM Tc.Type
unify info expected given = do
    expected' <- concreteType expected
    given' <- concreteType given
    if expected' == given'
        then return expected
        else throw $ ExpectedType info expected given

throw :: MonadWriter (DList FEError) m => FEError -> m a
throw e = tell (singleton e) >> pure (error "Evaluated failed type check thing")

-- NOTE: if transitive typedefs are allowed we should return the entire trace
-- instead of just the concrete type.
{- e.g
    typedef A B
    typedef B C
    typeDef C D
    concrete of A = D
    but we should have
    concrete of A = [B,C,D] maybe even [A,B,C,D] for convenience
-}
concreteType :: (MonadReader Ctx m, MonadWriter (DList FEError) m) => Tc.Type -> m Tc.Type
concreteType ty = do
    m <- asks typedefGraph
    traverseTypedefs ty m

traverseTypedefs :: (MonadWriter (DList FEError) m) => Tc.Type -> Map Tc.Type Tc.Type -> m Tc.Type
traverseTypedefs ty graph = go mempty ty
  where
    go :: (MonadWriter (DList FEError) m) => Set Tc.Type -> Tc.Type -> m Tc.Type
    go visited ty
        | ty `Set.member` visited = throw (TypeDefCircular ty)
        | otherwise = case Map.lookup ty graph of
            Nothing -> return ty
            Just ty' -> go (Set.insert ty visited) ty'

class TypeOf a where
    typeOf :: a -> Tc.Type

instance TypeOf Tc.Arg where
    typeOf (Tc.Argument ty _) = ty

instance TypeOf Tc.Expr where
    typeOf (t, _) = t

instance TypeOf Par.Arg where
    typeOf (Par.Argument _ typ _) = convert typ

pushExpr :: (MonadReader Ctx m) => Par.Expr -> m a -> m a
pushExpr e = local $ \s -> s {exprStack = e : s.exprStack}

pushDef :: (MonadReader Ctx m) => Par.Function -> m a -> m a
pushDef d = local $ \s -> s {defStack = d : s.defStack}
