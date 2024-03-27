{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TC.Tc where

import ParserTypes
import Ast
import Control.Arrow ((>>>))
import Control.Monad (unless, void, zipWithM_)
import Control.Monad.Except
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import Data.Map qualified as Map
import TC.Error
import TC.Types
import Utils

{-
TODO:
    - All calls to unify are potentially problematic.
        `int x = 5.5;` may perhaps type check because the order of the arguments matter, but we don't care atm
        fix this
    - Should probably create a tcExpr function again...
-}

tc :: ProgSyn -> Either String ProgTc
tc =
    tcProg
        >>> runTcM
        >>> flip evalStateT (Env mempty)
        >>> flip runReaderT (Ctx mempty mempty)
        >>> runExceptT
        >>> runIdity
        >>> \case
            Left err -> Left $ report err
            Right p -> Right p

tcProg :: ProgSyn -> TcM ProgTc
tcProg (ProgramSyn _ topdefs) = ProgramTc <$> mapM infDef topdefs

infDef :: TopDefSyn -> TcM TopDefTc
infDef def@(FnDefSyn p returnType name args block) = do
    let fnType = FunTc Nothing (convert returnType) (map typeOf args)
    insertVar (convert name) fnType
    blk <- pushDef def (inBlk (mapM_ insertArg args >> infBlk block))
    return (FnDefTc p (convert returnType) (convert name) (convert args) blk)

infStmt :: StmtSyn -> TcM StmtTc
infStmt = \case
    EmptySyn _ -> return EmptyTc
    DeclSyn pos typ items ->
        let typ' = convert typ
         in DeclTc pos typ' <$> mapM (tcItem typ') items
      where
        tcItem :: TypeTc -> ItemSyn -> TcM ItemTc
        tcItem expected = \case
            NoInitSyn pos name -> do
                let name' = convert name
                insertVar name' expected
                return $ NoInitTc pos name'
            InitSyn pos name expr -> do
                expr' <- infExpr expr
                void $ unify pos (typeOf expr') expected
                let name' = convert name
                insertVar name' expected
                return $ InitTc pos name' expr'
    AssSyn pos ident expr -> do
        typ <- lookupVar pos ident
        expr' <- infExpr expr
        void $ unify pos typ (typeOf expr')
        return $ AssTc pos typ (convert ident) expr'
    IncrSyn pos var -> do
        typ <- lookupVar pos var
        unless (isNumber typ) (throwError (TypeMismatch pos typ [double, int]))
        return (IncrTc pos (convert var))
    DecrSyn pos var -> do
        typ <- lookupVar pos var
        unless (isNumber typ) (throwError (TypeMismatch pos typ [double, int]))
        return (IncrTc pos (convert var))
    RetSyn pos expr -> do
        (FnDefSyn _ rt _ _ _) <- asks (head . defStack)
        expr' <- infExpr expr
        void $ unify pos (typeOf expr') (convert rt)
        return $ RetTc pos expr'
    VRetSyn pos -> do
        (FnDefSyn _ rt _ _ _) <- asks (head . defStack)
        unless (isVoid (convert @_ @TypeTc rt)) $ throwError (IllegalEmptyReturn pos (convert rt))
        return $ VRetTc pos
    CondSyn pos cond stmt -> do
        cond' <- infExpr cond
        let condType = typeOf cond'
        unless (isBool condType) $ throwError (TypeMismatch pos condType [bool])
        stmt' <- infStmt stmt
        return $ CondTc pos cond' stmt'
    CondElseSyn pos cond stmt1 stmt2 -> do
        cond' <- infExpr cond
        let condType = typeOf cond'
        unless (isBool condType) $ throwError (TypeMismatch pos condType [bool])
        stmt1' <- infStmt stmt1
        stmt2' <- infStmt stmt2
        return $ CondElseTc pos cond' stmt1' stmt2'
    WhileSyn pos cond stmt -> do
        cond' <- infExpr cond
        let condType = typeOf cond'
        unless (isBool condType) $ throwError (TypeMismatch pos condType [bool])
        stmt' <- infStmt stmt
        return $ WhileTc pos cond' stmt'
    SExpSyn pos expr -> SExpTc pos <$> infExpr expr

{- TODO: Type casting int to double is possible both ways I think, this should not be possible.
    And if it should be possible then the value needs to be floored.
-}
infExpr :: ExprSyn -> TcM ExprTc
infExpr e = pushExpr e $ case e of
    EVarSyn p i -> flip (EVarTc p) (convert i) <$> lookupVar p i
    ELitIntSyn p n -> return (ELitTc p int (LitIntTc n))
    ELitDoubleSyn p n -> return (ELitTc p double (LitDoubleTc n))
    ELitTrueSyn p -> return (ELitTc p bool (LitBoolTc True))
    ELitFalseSyn p -> return (ELitTc p bool (LitBoolTc False))
    EStringSyn p str -> return (ELitTc p string (LitStringTc str))
    NegSyn pos expr -> do
        infexpr <- infExpr expr
        let typ = typeOf infexpr
        unless (isNumber typ) (throwError (TypeMismatch pos typ [int, double]))
        return (NegTc pos typ infexpr)
    NotSyn pos expr -> do
        infexpr <- infExpr expr
        general <- unify pos (typeOf infexpr) bool
        return (NegTc pos general infexpr)
    EMulSyn p l op r -> do
        l' <- infExpr l
        r' <- infExpr r
        let typL = typeOf l'
        unless (isNumber typL) $ throwError (TypeMismatch p typL [int, double])
        let typR = typeOf r'
        unless (isNumber typR) $ throwError (TypeMismatch p typR [int, double])
        typ <- unify p typL typR
        return (EMulTc p typ l' (convert op) r')
    EAddSyn p l op r -> do
        l' <- infExpr l
        r' <- infExpr r
        let typL = typeOf l'
        unless (isNumber typL) $ throwError (TypeMismatch p typL [int, double])
        let typR = typeOf r'
        unless (isNumber typR) $ throwError (TypeMismatch p typR [int, double])
        typ <- unify p typL typR
        return (EAddTc p typ l' (convert op) r')
    EAndSyn p l r -> do
        l' <- infExpr l
        r' <- infExpr r
        let typL = typeOf l'
        unless (isBool typL) $ throwError (TypeMismatch p typL [bool])
        let typR = typeOf r'
        unless (isBool typR) $ throwError (TypeMismatch p typR [bool])
        return (EAndTc p l' r')
    EOrSyn p l r -> do
        l' <- infExpr l
        r' <- infExpr r
        let typL = typeOf l'
        unless (isBool typL) $ throwError (TypeMismatch p typL [bool])
        let typR = typeOf r'
        unless (isBool typR) $ throwError (TypeMismatch p typR [bool])
        return (EOrTc p l' r')
    ERelSyn p l op r -> do
        l' <- infExpr l
        r' <- infExpr r
        let comparableTypes = [string, bool, int, double] :: [TypeTc]
        let typL = typeOf l'
        unless (typL `elem` comparableTypes) $ throwError (NotComparable p typL)
        let typR = typeOf r'
        unless (typR `elem` comparableTypes) $ throwError (NotComparable p typR)
        void $ unify p typL typR
        return (ERelTc p l' (convert op) r')
    EAppSyn p ident exprs -> do
        rt <- lookupVar p ident
        case rt of
            FunTc _ rt argTypes -> do
                exprs' <- mapM infExpr exprs
                zipWithM_ (unify p) (fmap typeOf exprs') argTypes
                return (EAppTc p rt (convert ident) exprs')
            _ -> throwError (ExpectedFn p rt)

newtype Env = Env {variables :: [Map IdTc TypeTc]}
    deriving (Show, Eq, Ord)

data Ctx = Ctx {defStack :: [TopDefSyn], exprStack :: [ExprSyn]}
    deriving (Show, Eq, Ord)

newtype TcM a = TC {runTcM :: StateT Env (ReaderT Ctx (Except TcError)) a}
    deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadState Env, MonadError TcError)

-- | Extract the types from all top level definitions '⌣'
getDefs :: ProgSyn -> Map IdSyn (TypeTc, [TypeTc])
getDefs (ProgramSyn _ prog) =
    let argTypes = map (\(ArgumentSyn _ t _) -> t)
     in Map.fromList . for prog $ \(FnDefSyn _ rt ident args _) ->
            (ident, (convert rt, convert . argTypes $ args))

lookupVar :: InfoSyn -> IdSyn -> TcM TypeTc
lookupVar p i = do
    typ <- gets (Map.lookup (convert i) . head . variables)
    case typ of
        Just rt -> return rt
        _ -> throwError (UnboundVariable p (convert i))

insertArg :: ArgSyn -> TcM ()
insertArg (ArgumentSyn _ typ name) = insertVar (convert name) (convert typ)

insertVar :: IdTc -> TypeTc -> TcM ()
insertVar name typ = do
    blocks <- gets variables
    case blocks of
        [] -> modify (\s -> s {variables = [Map.singleton name typ]})
        (x : xs) -> modify (\s -> s {variables = Map.insert name typ x : xs})

{-| Type class to help converting from the parser types
  to the type checker type
-}
class Convert a b where
    convert :: a -> b

instance (Convert a b) => Convert [a] [b] where
    convert = map convert

instance Convert TypeSyn TypeTc where
    convert = \case
        TVarSyn pos a -> TVarTc pos a
        FunSyn pos rt args -> FunTc pos (convert rt) (convert args)

instance Convert IdSyn IdTc where
    convert (IdSyn _ s) = IdTc s

instance Convert MulOpSyn MulOpTc where
    convert = \case
        TimesSyn p -> TimesTc p
        DivSyn p -> DivTc p
        ModSyn p -> ModTc p

instance Convert AddOpSyn AddOpTc where
    convert = \case
        PlusSyn p -> PlusTc p
        MinusSyn p -> MinusTc p

instance Convert RelOpSyn RelOpTc where
    convert = \case
        LTHSyn p -> LTHTc p
        LESyn p -> LETc p
        GTHSyn p -> GTHTc p
        GESyn p -> GETc p
        EQUSyn p -> EQUTc p
        NESyn p -> NETc p

class TypeOf a where
    typeOf :: a -> TypeTc

instance TypeOf ExprTc where
    typeOf = \case
        EVarTc _ t -> t
        NegTc _ t -> t
        NotTc _ _ -> BoolTc Nothing
        EAppTc _ rt _ -> rt
        EMulTc _ t _ _ -> t
        EAddTc _ t _ _ -> t
        ERelTc {} -> BoolTc Nothing
        EAndTc {} -> BoolTc Nothing
        EOrTc {} -> BoolTc Nothing

instance TypeOf ArgSyn where
    typeOf (ArgumentSyn _ typ _) = convert typ

unify :: (MonadError TcError m) => InfoSyn -> TypeTc -> TypeTc -> m TypeTc
unify _ given expected
    | delPos given == delPos expected = return given
    | otherwise = case (given, expected) of
        (TVarSyn _ "int", TVarSyn _ "double") -> return $ DoubleTc Nothing
        _ -> throwError (TypeMismatch pos given [expected])

isNumber :: TypeTc -> Bool
isNumber (TVarSyn _ "int") = True
isNumber (TVarSyn _ "double") = True
isNumber _ = False

isBool :: TypeTc -> Bool
isBool (TVarSyn _ "boolean") = True
isBool _ = False

isInt :: TypeTc -> Bool
isInt (TVarSyn _ "int") = True
isInt _ = False

isDouble :: TypeTc' a -> Bool
isDouble (TVarSyn _ "double") = True
isDouble _ = False

isString :: TypeTc' a -> Bool
isString (TVarSyn _ "string") = True
isString _ = False

isFun :: TypeTc' a -> Bool
isFun (FunTc {}) = True
isFun _ = False

isVoid :: TypeTc' a -> Bool
isVoid (TVarSyn _ "void") = True
isVoid _ = False

int :: TypeTc
int = IntTc Nothing

double :: TypeTc
double = DoubleTc Nothing

string :: TypeTc
string = StringTc Nothing

bool :: TypeTc
bool = BoolTc Nothing

pushExpr :: (MonadReader Ctx m) => ExprSyn -> m a -> m a
pushExpr e = local $ \s -> s {exprStack = e : s.exprStack}

pushDef :: (MonadReader Ctx m) => TopDefSyn -> m a -> m a
pushDef d = local $ \s -> s {defStack = d : s.defStack}

inBlk :: TcM a -> TcM a
inBlk ma = do
    pushBlk
    x <- ma
    popBlk
    return x

pushBlk :: TcM ()
pushBlk = modify (\s -> s {variables = mempty : s.variables})

popBlk :: TcM ()
popBlk = do
    gets variables >>= \case
        [] -> return ()
        (_ : xs) -> modify (\s -> s {variables = xs})
