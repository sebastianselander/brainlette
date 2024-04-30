{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Tc.Tc where

import Control.Arrow (first, (>>>))
import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Tuple.Extra (uncurry3)
import Frontend.Error
import Frontend.Parser.ParserTypes qualified as Par
import Frontend.Tc.Types qualified as Tc

tc :: Par.Prog -> Either Text Tc.Prog
tc p =
    tcProg
        >>> runTcM
        >>> flip evalStateT (addDefs p)
        >>> flip
            runReaderT
            initCtx
        >>> runExceptT
        >>> runIdentity
        >>> \case
            Left err -> Left $ report err
            Right p -> Right p
        $ p

initCtx :: Ctx
initCtx = Ctx mempty mempty (Map.singleton Tc.Int [Tc.Int])

initEnv :: Env
initEnv = Env mempty mempty

run :: TcM a -> Either Text a
run =
    runTcM
        >>> flip evalStateT initEnv
        >>> flip
            runReaderT
            initCtx
        >>> runExceptT
        >>> runIdentity
        >>> \case
            Left err -> Left $ report err
            Right p -> Right p

tcProg :: Par.Prog -> TcM Tc.Prog
tcProg (Par.Program _ defs) = Tc.Program <$> mapM infDef defs

infDef :: Par.TopDef -> TcM Tc.TopDef
infDef = \case
    def@(Par.FnDef _ rt name args block) -> do
        let rt' = convert rt
        let name' = convert name
        let fnType = Tc.Fun rt' (map typeOf args)
        insertFunc name' fnType
        block' <- pushDef def $ do
            mapM_ (insertArg name) args
            mapMaybeM (tcStmt rt') block
        return (Tc.FnDef rt' name' (convert args) block')

tcStmt :: Tc.Type -> Par.Stmt -> TcM (Maybe Tc.Stmt)
tcStmt retTy = go
  where
    go :: Par.Stmt -> TcM (Maybe Tc.Stmt)
    go s = case s of
        Par.Empty _ -> return Nothing
        Par.BStmt _ stmts -> Just . Tc.BStmt <$> mapMaybeM go stmts
        Par.Decl info typ items -> do
            let typ' = convert typ
            when (isVoid typ') (throwError $ VoidDeclare info s)
            Just . Tc.Decl typ' <$> mapM (tcItem info typ') items
          where
            tcItem :: Par.SynInfo -> Tc.Type -> Par.Item -> TcM Tc.Item
            tcItem info expected = \case
                Par.NoInit _ name -> do
                    let name' = convert name
                    exist <- doesVariableExist name'
                    when exist $ throwError (BoundVariable info name')
                    insertVar name' expected
                    return $ Tc.NoInit name'
                Par.Init _ name expr -> do
                    let name' = convert name
                    exist <- doesVariableExist name'
                    when exist $ throwError (BoundVariable info name')
                    (ty, expr') <- infExpr expr
                    ty' <- unify info expected ty
                    insertVar name' ty
                    return $ Tc.Init name' (ty', expr')
        Par.Ass info ident expr -> do
            identTy <- getVar ident
            (ty, expr') <- infExpr expr
            ty' <- unify info identTy ty
            return (Just (Tc.Ass (convert ident) (ty', expr')))
        Par.Incr info var -> do
            typ <- getVar var
            unless (isInt typ) (throwError $ ExpectedType info Tc.Int typ)
            errNotNumber info typ
            return (Just (Tc.Incr typ (convert var)))
        Par.Decr info var -> do
            typ <- getVar var
            errNotNumber info typ
            return (Just (Tc.Incr typ (convert var)))
        Par.Ret info expr -> do
            (ty, expr') <- infExpr expr
            ty' <- unify info retTy ty
            return (Just (Tc.Ret (ty', expr')))
        Par.VRet pos -> do
            unless (isVoid retTy) $ throwError (IllegalEmptyReturn pos retTy)
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
        Par.SExp _ expr@(Par.EApp {}) -> Just . Tc.SExp <$> infExpr expr
        Par.SExp info expr -> throwError (NotStatement info expr)
        Par.Break _ -> return $ Just Tc.Break

infExpr :: Par.Expr -> TcM Tc.Expr
infExpr e = pushExpr e $ case e of
    Par.ELitInt _ n -> return (Tc.Int, Tc.ELit (Tc.LitInt n))
    Par.ELitDouble _ n -> return (Tc.Double, Tc.ELit (Tc.LitDouble n))
    Par.ELitTrue _ -> return (Tc.Boolean, Tc.ELit (Tc.LitBool True))
    Par.ELitFalse _ -> return (Tc.Boolean, Tc.ELit (Tc.LitBool False))
    Par.EString _ str -> return (Tc.String, Tc.ELit (Tc.LitString str))
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
        -- TODO: refactor for adding more number types
        ty <- case (tl, tr) of
            (Tc.Double, Tc.Int) -> return Tc.Double
            (Tc.Int, Tc.Double) -> return Tc.Double
            (Tc.Int, Tc.Int) -> return Tc.Int
            (Tc.Double, Tc.Double) -> return Tc.Double
            (Tc.Boolean, Tc.Boolean) -> return Tc.Boolean
            (l, r)
                | l == r -> throwError (NotRelational info l)
                | otherwise -> throwError (TypeMismatch info l [r])
        return (Tc.Boolean, Tc.ERel (ty, l') (convert op) (ty, r'))
    Par.EApp p ident exprs -> do
        ty <- getVar ident
        case ty of
            Tc.Fun rt argtys -> do
                let infos = map hasInfo exprs
                exprs' <- mapM infExpr exprs
                let expecteds = length argtys
                    givens = length exprs'
                unless (expecteds == givens) $
                    throwError (ArgumentMismatch p ident expecteds givens)
                let infoTys = zip3 infos argtys (map typeOf exprs')
                mapM_ (uncurry3 unify) infoTys
                return (rt, Tc.EApp (convert ident) exprs')
            _else -> throwError (ExpectedFn p ty)

data Env = Env {variables :: Map Tc.Id Tc.Type, functions :: Map Tc.Id Tc.Type}
    deriving (Show, Eq, Ord)

data Ctx = Ctx
    { defStack :: [Par.TopDef]
    , exprStack :: [Par.Expr]
    , subtypes :: Map Tc.Type [Tc.Type]
    }
    deriving (Show, Eq, Ord)

newtype TcM a = TC {runTcM :: StateT Env (ReaderT Ctx (Except FEError)) a}
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader Ctx
        , MonadState Env
        , MonadError FEError
        )

-- | Extract the types from all top level definitions '⌣'
addDefs :: Par.Prog -> Env
addDefs (Par.Program _ prog) =
    let prelude =
            [ (Tc.Id "printInt", Tc.Fun Tc.Void [Tc.Int])
            , (Tc.Id "printDouble", Tc.Fun Tc.Void [Tc.Double])
            , (Tc.Id "printString", Tc.Fun Tc.Void [Tc.String])
            , (Tc.Id "readInt", Tc.Fun Tc.Int [])
            , (Tc.Id "readDouble", Tc.Fun Tc.Double [])
            , (Tc.Id "readString", Tc.Fun Tc.String [])
            ]
     in Env mempty (Map.fromList $ prelude <> map (first convert . getType) prog)
  where
    getType (Par.FnDef _ ty name args _) =
        (name, Tc.Fun (convert ty) (map typeOf args))

lookupFunc :: Par.Id -> TcM (Maybe Tc.Type)
lookupFunc i = do
    gets (Map.lookup (convert i) . functions) >>= \case
        Nothing -> return Nothing
        Just rt -> return (return rt)

lookupVar :: Par.Id -> TcM (Maybe Tc.Type)
lookupVar i = do
    gets (Map.lookup (convert i) . variables) >>= \case
        Nothing -> return Nothing
        Just rt -> return (return rt)

getVar :: Par.Id -> TcM Tc.Type
getVar i@(Par.Id _ _i) = do
    mbTy <- gets (Map.lookup (convert i) . variables)
    case mbTy of
        Nothing ->
            gets
                ( fromMaybe
                    ( error $
                        "TYPECHECKER: Variable lookup bug. Please report\nCould not find name: " <> unpack _i <> "\n"
                    )
                    . Map.lookup (convert i)
                    . functions
                )
        Just ty -> return ty

doesVariableExist :: Tc.Id -> TcM Bool
doesVariableExist name = gets (Map.member name . variables)

insertArg :: Par.Id -> Par.Arg -> TcM ()
insertArg ident (Par.Argument info (Par.Void _) _) = throwError $ VoidParameter info ident
insertArg _ (Par.Argument _ ty name) = insertVar (convert name) (convert ty)

insertVar :: Tc.Id -> Tc.Type -> TcM ()
insertVar name typ = modify (\s -> s {variables = Map.insert name typ s.variables})

insertFunc :: Tc.Id -> Tc.Type -> TcM ()
insertFunc name typ = modify (\s -> s {functions = Map.insert name typ s.functions})

errNotBoolean :: (MonadError FEError m) => Par.SynInfo -> Tc.Type -> m ()
errNotBoolean info = \case
    Tc.Boolean -> return ()
    other -> throwError (ExpectedType info Tc.Boolean other)

errNotNumber :: (MonadError FEError m) => Par.SynInfo -> Tc.Type -> m ()
errNotNumber info ty
    | isNumber ty = return ()
    | otherwise = throwError (ExpectedNumber info ty)

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
    tys <- getSubtypes given
    if expected `elem` tys
        then return expected
        else throwError $ ExpectedType info expected given

-- | Relation for if expected is a subtype of given
getSubtypes :: Tc.Type -> TcM [Tc.Type]
getSubtypes expected =
    asks (Map.findWithDefault [expected] expected . subtypes)

class HasInfo a where
    hasInfo :: a -> Par.SynInfo

instance HasInfo Par.TopDef where
  hasInfo = \case
    Par.FnDef i _ _ _ _ -> i

instance HasInfo Par.Stmt where
  hasInfo = \case
           Par.Empty i -> i
           Par.BStmt i _ -> i
           Par.Decl i _ _ -> i
           Par.Ass i _ _ -> i
           Par.Incr i _ -> i
           Par.Decr i _ -> i
           Par.Ret i _ -> i
           Par.VRet i -> i
           Par.Cond i _ _ -> i
           Par.CondElse i _ _ _ -> i
           Par.While i _ _ -> i
           Par.Break i -> i
           Par.SExp i _ -> i

instance HasInfo Par.Expr where
    hasInfo = \case
        Par.EVar i _ -> i
        Par.ELitInt i _ -> i
        Par.ELitDouble i _ -> i
        Par.ELitTrue i -> i
        Par.ELitFalse i -> i
        Par.EApp i _ _ -> i
        Par.EString i _ -> i
        Par.Neg i _ -> i
        Par.Not i _ -> i
        Par.EMul i _ _ _ -> i
        Par.EAdd i _ _ _ -> i
        Par.ERel i _ _ _ -> i
        Par.EAnd i _ _ -> i
        Par.EOr i _ _ -> i

class TypeOf a where
    typeOf :: a -> Tc.Type

instance TypeOf Tc.Expr where
    typeOf (t, _) = t

instance TypeOf Par.Arg where
    typeOf (Par.Argument _ typ _) = convert typ

pushExpr :: (MonadReader Ctx m) => Par.Expr -> m a -> m a
pushExpr e = local $ \s -> s {exprStack = e : s.exprStack}

pushDef :: (MonadReader Ctx m) => Par.TopDef -> m a -> m a
pushDef d = local $ \s -> s {defStack = d : s.defStack}
