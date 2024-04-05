{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Tuple.Extra (uncurry3)
import ParserTypes qualified as Par
import Frontend.Error
import Frontend.Tc.Types qualified as Tc

tc :: Par.Prog -> Either Text Tc.Prog
tc p =
    tcProg
        >>> runTcM
        >>> flip evalStateT (addDefs p)
        >>> flip
            runReaderT
            (Ctx mempty mempty (Map.singleton Tc.Int [Tc.Double, Tc.Int]))
        >>> runExceptT
        >>> runIdentity
        >>> \case
            Left err -> Left $ report err
            Right p -> Right p
        $ p

run :: TcM a -> Either Text a
run =
    runTcM
        >>> flip evalStateT (Env mempty)
        >>> flip
            runReaderT
            (Ctx mempty mempty (Map.singleton Tc.Int [Tc.Double, Tc.Int]))
        >>> runExceptT
        >>> runIdentity
        >>> \case
            Left err -> Left $ report err
            Right p -> Right p

tcProg :: Par.Prog -> TcM Tc.Prog
tcProg (Par.Program _ defs) = Tc.Program <$> mapM infDef defs

infDef :: Par.TopDef -> TcM Tc.TopDef
infDef def@(Par.FnDef _ rt name args block) = do
    let rt' = convert rt
    let name' = convert name
    let fnType = Tc.Fun rt' (map typeOf args)
    insertVar name' fnType
    block' <- pushDef def $ inBlock $ do
        mapM_ insertArg args
        mapMaybeM infStmt block
    return (Tc.FnDef rt' name' (convert args) block')

infStmt :: Par.Stmt -> TcM (Maybe Tc.Stmt)
infStmt = \case
    Par.Empty _ -> return Nothing
    Par.BStmt _ stmts -> Just . Tc.BStmt <$> inBlock (mapMaybeM infStmt stmts)
    Par.Decl info typ items -> do
        let typ' = convert typ
         in Just . Tc.Decl typ' <$> mapM (tcItem info typ') items
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
        identTy <- lookupVar info ident
        (ty, expr') <- infExpr expr
        ty' <- unify info identTy ty
        return (Just (Tc.Ass (convert ident) (ty', expr')))
    Par.Incr info var -> do
        typ <- lookupVar info var
        errNotNumber info typ
        return (Just (Tc.Incr typ (convert var)))
    Par.Decr info var -> do
        typ <- lookupVar info var
        errNotNumber info typ
        return (Just (Tc.Incr typ (convert var)))
    Par.Ret info expr -> do
        (Par.FnDef _ rt _ _ _) <- asks (head . defStack)
        (ty, expr') <- infExpr expr
        ty' <- unify info (convert rt) ty
        return (Just (Tc.Ret (ty', expr')))
    Par.VRet pos -> do
        (Par.FnDef _ rt _ _ _) <- asks (head . defStack)
        unless (isVoid (convert rt)) $
            throwError (IllegalEmptyReturn pos (convert rt))
        return (Just Tc.VRet)
    Par.Cond _ cond stmt -> do
        cond' <- infExpr cond
        errNotBoolean (hasInfo cond) (typeOf cond')
        stmt' <- infStmt stmt
        let statement = fromMaybe (Tc.BStmt []) stmt'
        return $ Just (Tc.Cond cond' statement)
    Par.CondElse _ cond stmt1 stmt2 -> do
        cond' <- infExpr cond
        errNotBoolean (hasInfo cond) (typeOf cond')
        stmt1' <- fromMaybe (Tc.BStmt []) <$> infStmt stmt1
        stmt2' <- fromMaybe (Tc.BStmt []) <$> infStmt stmt2
        return (Just (Tc.CondElse cond' stmt1' stmt2'))
    Par.While _ cond stmt -> do
        cond' <- infExpr cond
        errNotBoolean (hasInfo cond) (typeOf cond')
        stmt' <- fromMaybe (Tc.BStmt []) <$> infStmt stmt
        return (Just (Tc.While cond' stmt'))
    Par.SExp _ expr -> Just . Tc.SExp <$> infExpr expr
    Par.Break _ -> return $ Just Tc.Break


infExpr :: Par.Expr -> TcM Tc.Expr
infExpr e = pushExpr e $ case e of
    Par.ELitInt _ n -> return (Tc.Int, Tc.ELit (Tc.LitInt n))
    Par.ELitDouble _ n -> return (Tc.Double, Tc.ELit (Tc.LitDouble n))
    Par.ELitTrue _ -> return (Tc.Boolean, Tc.ELit (Tc.LitBool True))
    Par.ELitFalse _ -> return (Tc.Boolean, Tc.ELit (Tc.LitBool False))
    Par.EString _ str -> return (Tc.String, Tc.ELit (Tc.LitString str))
    Par.EVar p i -> do
        ty <- lookupVar p i
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
        (tyl, l') <- infExpr l
        errNotNumber info tyl
        (tyr, r') <- infExpr r
        errNotNumber info tyr
        let ty = case (tyl, tyr) of
                (Tc.Double, _) -> Tc.Double
                (_, Tc.Double) -> Tc.Double
                _ -> tyl
        return (ty, Tc.EMul (ty, l') (convert op) (ty, r'))
    Par.EAdd info l op r -> do
        (tyl, l') <- infExpr l
        errNotNumber info tyl
        (tyr, r') <- infExpr r
        errNotNumber info tyr
        let ty = case (tyl, tyr) of
                (Tc.Double, _) -> Tc.Double
                (_, Tc.Double) -> Tc.Double
                _ -> tyl
        return (ty, Tc.EAdd (ty, l') (convert op) (ty, r'))
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
            (l, r)
                | l == r -> return l
                | otherwise -> throwError (TypeMismatch info l [r])
        return (Tc.Boolean, Tc.ERel (ty, l') (convert op) (ty, r'))
    Par.EApp p ident exprs -> do
        ty <- lookupVar p ident
        case ty of
            Tc.Fun rt argtys -> do
                let infos = map hasInfo exprs
                exprs' <- mapM infExpr exprs
                let infoTys = zip3 infos argtys (map typeOf exprs')
                mapM_ (uncurry3 unify) infoTys
                return (rt, Tc.EApp (convert ident) exprs')
            _else -> throwError (ExpectedFn p ty)

newtype Env = Env {variables :: [Map Tc.Id Tc.Type]}
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
    Env [Map.fromList $ map (first convert . getType) prog]
  where
    getType (Par.FnDef _ ty name args _) =
        (name, Tc.Fun (convert ty) (map typeOf args))

lookupVar :: Par.SynInfo -> Par.Id -> TcM Tc.Type
lookupVar info i = do
    vars <- gets variables
    findVar vars
  where
    findVar [] = throwError (UnboundVariable info (convert i))
    findVar (vs : vvs) = case Map.lookup (convert i) vs of
        Just rt -> return rt
        Nothing -> findVar vvs

doesVariableExist :: Tc.Id -> TcM Bool
doesVariableExist name =
    gets (maybe False (Map.member name) . listToMaybe . variables)

insertArg :: Par.Arg -> TcM ()
insertArg (Par.Argument _ typ name) = insertVar (convert name) (convert typ)

insertVar :: Tc.Id -> Tc.Type -> TcM ()
insertVar name typ = do
    blocks <- gets variables
    case blocks of
        [] -> modify (\s -> s {variables = [Map.singleton name typ]})
        (x : xs) -> modify (\s -> s {variables = Map.insert name typ x : xs})

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

{-| Type class to help converting from the parser types
  to the type checker type
-}
class Convert a b where
    convert :: a -> b

instance (Convert a b) => Convert [a] [b] where
    convert = map convert

instance Convert Par.Type Tc.Type where
    convert = \case
        Par.TVar _ t -> Tc.TVar (convert t)
        Par.Fun _ rt argtys -> Tc.Fun (convert rt) (convert argtys)

instance Convert Par.Id Tc.Id where
    convert (Par.Id _ s) = Tc.Id s

instance Convert Par.MulOp Tc.MulOp where
    convert = \case
        Par.Times _ -> Tc.Times
        Par.Div _ -> Tc.Div
        Par.Mod _ -> Tc.Mod

instance Convert Par.AddOp Tc.AddOp where
    convert = \case
        Par.Plus _ -> Tc.Plus
        Par.Minus _ -> Tc.Minus

instance Convert Par.RelOp Tc.RelOp where
    convert = \case
        Par.LTH _ -> Tc.LTH
        Par.LE _ -> Tc.LE
        Par.GTH _ -> Tc.GTH
        Par.GE _ -> Tc.GE
        Par.EQU _ -> Tc.EQU
        Par.NE _ -> Tc.NE

instance Convert Par.Arg Tc.Arg where
    convert = \case
        Par.Argument _ typ name -> Tc.Argument (convert typ) (convert name)

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

inBlock :: TcM a -> TcM a
inBlock ma = do
    pushBlk
    x <- ma
    popBlk
    return x
  where
    pushBlk :: TcM ()
    pushBlk = modify (\s -> s {variables = mempty : s.variables})

    popBlk :: TcM ()
    popBlk =
        gets variables >>= \case
            [] -> return ()
            (_ : xs) -> modify (\s -> s {variables = xs})
