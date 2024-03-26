{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TC.Typechecker where

import Brainlette.Abs qualified as Par
import Control.Arrow ((>>>))
import Control.Monad (unless, void, zipWithM_)
import Control.Monad.Except
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import Data.Map qualified as Map
import TC.Error
import TC.Types qualified as Tc
import Utils
import Debug.Trace (traceShowId, traceShowM)


{-
TODO:
    - All calls to unify are potentially problematic.
        `int x = 5.5;` may perhaps type check because the order of the arguments matter, but we don't care atm
        fix this
    - Should probably create a tcExpr function again...
-}

tc :: Par.Prog -> Either TcError Tc.Prog
tc =
    tcProg
        >>> runTcM
        >>> flip evalStateT (Env mempty)
        >>> flip runReaderT (Ctx mempty mempty)
        >>> runExceptT
        >>> runIdentity

tcProg :: Par.Prog -> TcM Tc.Prog
tcProg (Par.Program _ topdefs) = Tc.Program <$> mapM infDef topdefs

infDef :: Par.TopDef -> TcM Tc.TopDef
infDef (Par.FnDef p returnType name args block) = do
    let fnType = Tc.Fun Nothing (convert returnType) (map typeOf args)
    insertVar (convert name) fnType
    blk <- inBlk $ do
        mapM_ insertArg args
        infBlk block
    return (Tc.FnDef p (convert returnType) (convert name) (convert args) blk)

infBlk :: Par.Blk -> TcM Tc.Blk
infBlk (Par.Block pos stmts) = Tc.Block pos <$> mapM infStmt stmts

infStmt :: Par.Stmt -> TcM Tc.Stmt
infStmt = \case
    Par.Empty _ -> return Tc.Empty
    Par.BStmt pos (Par.Block bpos stmts) ->
        Tc.BStmt pos . Tc.Block bpos <$> mapM infStmt stmts
    Par.Decl pos typ items ->
        let typ' = convert typ
         in Tc.Decl pos typ' <$> mapM (tcItem typ') items
      where
        tcItem :: Tc.Type -> Par.Item -> TcM Tc.Item
        tcItem expected = \case
            Par.NoInit pos name -> do
                let name' = convert name
                insertVar name' expected
                return $ Tc.NoInit pos name'
            Par.Init pos name expr -> do
                expr' <- infExpr expr
                traceShowM expr'
                void $ unify pos (typeOf expr') expected
                let name' = convert name
                insertVar name' expected
                return $ Tc.Init pos name' expr'
    Par.Ass pos ident expr -> do
        typ <- lookupVar pos ident
        expr' <- infExpr expr
        void $ unify pos typ (typeOf expr')
        return $ Tc.Ass pos typ (convert ident) expr'
    Par.Incr pos var -> do
        typ <- lookupVar pos var
        unless (isNumber typ) (throwError (TypeMismatch pos typ [double, int]))
        return (Tc.Incr pos (convert var))
    Par.Decr pos var -> do
        typ <- lookupVar pos var
        unless (isNumber typ) (throwError (TypeMismatch pos typ [double, int]))
        return (Tc.Incr pos (convert var))
    Par.Ret pos expr -> undefined
    Par.VRet pos -> undefined
    Par.Cond pos cond stmt -> do
        cond' <- infExpr cond
        let condType = typeOf cond'
        unless (isBool condType) $ throwError (TypeMismatch pos condType [bool])
        stmt' <- infStmt stmt
        return $ Tc.Cond pos cond' stmt'
    Par.CondElse pos cond stmt1 stmt2 -> do
        cond' <- infExpr cond
        let condType = typeOf cond'
        unless (isBool condType) $ throwError (TypeMismatch pos condType [bool])
        stmt1' <- infStmt stmt1
        stmt2' <- infStmt stmt2
        return $ Tc.CondElse pos cond' stmt1' stmt2'
    Par.While pos cond stmt -> do
        cond' <- infExpr cond
        let condType = typeOf cond'
        unless (isBool condType) $ throwError (TypeMismatch pos condType [bool])
        stmt' <- infStmt stmt
        return $ Tc.While pos cond' stmt'
    Par.SExp pos expr -> Tc.SExp pos <$> infExpr expr

{- TODO: Type casting int to double is possible both ways I think, this should not be possible.
    And if it should be possible then the value needs to be floored.
-}
infExpr :: Par.Expr -> TcM Tc.Expr
infExpr = \case
    Par.EVar p i -> flip (Tc.EVar p) (convert i) <$> lookupVar p i
    Par.ELitInt p n -> return (Tc.ELit p int (Tc.LitInt n))
    Par.ELitDoub p n -> return (Tc.ELit p double (Tc.LitDouble n))
    Par.ELitTrue p -> return (Tc.ELit p bool (Tc.LitBool True))
    Par.ELitFalse p -> return (Tc.ELit p bool (Tc.LitBool False))
    Par.EString p str -> return (Tc.ELit p string (Tc.LitString str))
    Par.Neg pos expr -> do
        infexpr <- infExpr expr
        let typ = typeOf infexpr
        unless (isNumber typ) (throwError (TypeMismatch pos typ [int, double]))
        return (Tc.Neg pos typ infexpr)
    Par.Not pos expr -> do
        infexpr <- infExpr expr
        general <- unify pos (typeOf infexpr) bool
        return (Tc.Neg pos general infexpr)
    Par.EMul p l op r -> do
        l' <- infExpr l
        r' <- infExpr r
        let typL = typeOf l'
        unless (isNumber typL) $ throwError (TypeMismatch p typL [int, double])
        let typR = typeOf r'
        unless (isNumber typR) $ throwError (TypeMismatch p typR [int, double])
        typ <- unify p typL typR
        return (Tc.EMul p typ l' (convert op) r')
    Par.EAdd p l op r -> do
        l' <- infExpr l
        r' <- infExpr r
        let typL = typeOf l'
        unless (isNumber typL) $ throwError (TypeMismatch p typL [int, double])
        let typR = typeOf r'
        unless (isNumber typR) $ throwError (TypeMismatch p typR [int, double])
        typ <- unify p typL typR
        return (Tc.EAdd p typ l' (convert op) r')
    Par.EAnd p l r -> do
        l' <- infExpr l
        r' <- infExpr r
        let typL = typeOf l'
        unless (isBool typL) $ throwError (TypeMismatch p typL [bool])
        let typR = typeOf r'
        unless (isBool typR) $ throwError (TypeMismatch p typR [bool])
        return (Tc.EAnd p l' r')
    Par.EOr p l r -> do
        l' <- infExpr l
        r' <- infExpr r
        let typL = typeOf l'
        unless (isBool typL) $ throwError (TypeMismatch p typL [bool])
        let typR = typeOf r'
        unless (isBool typR) $ throwError (TypeMismatch p typR [bool])
        return (Tc.EOr p l' r')
    Par.ERel p l op r -> do
        l' <- infExpr l
        r' <- infExpr r
        let comparableTypes = [string, bool, int, double]
        let typL = typeOf l'
        unless (typL `elem` comparableTypes) $ throwError (NotComparable p typL)
        let typR = typeOf r'
        unless (typR `elem` comparableTypes) $ throwError (NotComparable p typR)
        void $ unify p typL typR
        return (Tc.ERel p l' (convert op) r')
    Par.EApp p ident exprs -> do
        rt <- lookupVar p ident
        case rt of
            Tc.Fun _ rt argTypes -> do
                exprs' <- mapM infExpr exprs
                zipWithM_ (unify p) (fmap typeOf exprs') argTypes
                return (Tc.EApp p rt (convert ident) exprs')
            _ -> throwError (ExpectedFn p rt)

newtype Env = Env {variables :: [Map Tc.Ident Tc.Type]}
    deriving (Show, Eq, Ord)

data Ctx = Ctx {defStack :: [Par.TopDef], exprStack :: [Par.Expr]}
    deriving (Show, Eq, Ord)

newtype TcM a = TC {runTcM :: StateT Env (ReaderT Ctx (Except TcError)) a}
    deriving (Functor, Applicative, Monad, MonadState Env, MonadError TcError)

-- | Extract the types from all top level definitions '⌣'
getDefs :: Par.Prog -> Map Par.Ident (Tc.Type, [Tc.Type])
getDefs (Par.Program _ prog) =
    let argTypes = map (\(Par.Argument _ t _) -> t)
     in Map.fromList . for prog $ \(Par.FnDef _ rt ident args _) ->
            (ident, (convert rt, convert . argTypes $ args))

lookupVar :: Tc.Position -> Par.Ident -> TcM Tc.Type
lookupVar p i = do
    typ <- gets (Map.lookup (convert i) . head . variables)
    case typ of
        Just rt -> return rt
        _ -> throwError (UnboundVariable p (convert i))

insertArg :: Par.Arg -> TcM ()
insertArg (Par.Argument _ typ name) = insertVar (convert name) (convert typ)

insertVar :: Tc.Ident -> Tc.Type -> TcM ()
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

instance Convert Par.Type Tc.Type where
    convert = \case
        Par.Int pos -> Tc.Int pos
        Par.Doub pos -> Tc.Double pos
        Par.Bool pos -> Tc.Bool pos
        Par.Void pos -> Tc.Void pos
        Par.Fun pos rt args -> Tc.Fun pos (convert rt) (convert args)

instance Convert Par.Ident Tc.Ident where
    convert (Par.Ident s) = Tc.Ident s

instance Convert Par.MulOp Tc.MulOp where
    convert = \case
        Par.Times p -> Tc.Times p
        Par.Div p -> Tc.Div p
        Par.Mod p -> Tc.Mod p

instance Convert Par.AddOp Tc.AddOp where
    convert = \case
        Par.Plus p -> Tc.Plus p
        Par.Minus p -> Tc.Minus p

instance Convert Par.RelOp Tc.RelOp where
    convert = \case
        Par.LTH p -> Tc.LTH p
        Par.LE p -> Tc.LE p
        Par.GTH p -> Tc.GTH p
        Par.GE p -> Tc.GE p
        Par.EQU p -> Tc.EQU p
        Par.NE p -> Tc.NE p

instance Convert Par.Arg Tc.Arg where
    convert = \case
        Par.Argument pos typ name -> Tc.Argument pos (convert typ) (convert name)

class TypeOf a where
    typeOf :: a -> Tc.Type

instance TypeOf Tc.Expr where
    typeOf = \case
        Tc.EVar _ t _ -> t
        Tc.ELit _ t _ -> t
        Tc.Neg _ t _ -> t
        Tc.Not _ _ -> Tc.Bool Nothing
        Tc.EApp _ rt _ _ -> rt
        Tc.EMul _ t _ _ _ -> t
        Tc.EAdd _ t _ _ _ -> t
        Tc.ERel {} -> Tc.Bool Nothing
        Tc.EAnd {} -> Tc.Bool Nothing
        Tc.EOr {} -> Tc.Bool Nothing

instance TypeOf Par.Arg where
    typeOf (Par.Argument _ typ _) = convert typ

delPos :: Functor f => f Tc.Position -> f ()
delPos = void

unify :: (MonadError TcError m) => Tc.Position -> Tc.Type -> Tc.Type -> m Tc.Type
unify pos l r
    | delPos l == delPos r = return l
    | otherwise = case (l, r) of
        (Tc.Int _, Tc.Double _) -> return $ Tc.Double Nothing
        (Tc.Double _, Tc.Int _) -> return $ Tc.Double Nothing
        _ -> throwError (TypeMismatch pos l [r])

isNumber :: Tc.Type -> Bool
isNumber (Tc.Double _) = True
isNumber (Tc.Int _) = True
isNumber (Tc.String _) = False
isNumber (Tc.Bool _) = False
isNumber (Tc.Fun {}) = False
isNumber (Tc.Void _) = False

isBool :: Tc.Type -> Bool
isBool (Tc.Bool _) = True
isBool _ = False

isInt :: Tc.Type -> Bool
isInt (Tc.Int _) = True
isInt _ = False

isDouble :: Tc.Type' a -> Bool
isDouble (Tc.Double _) = True
isDouble _ = False

isString :: Tc.Type' a -> Bool
isString (Tc.String _) = True
isString _ = False

isFun :: Tc.Type' a -> Bool
isFun (Tc.Fun {}) = True
isFun _ = False

isVoid :: Tc.Type' a -> Bool
isVoid (Tc.Void _) = True
isVoid _ = False

int :: Tc.Type
int = Tc.Int Nothing

double :: Tc.Type
double = Tc.Double Nothing

string :: Tc.Type
string = Tc.String Nothing

bool :: Tc.Type
bool = Tc.Bool Nothing

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
