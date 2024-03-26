{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TC.Typechecker where

import Brainlette.Abs qualified as Par
import Control.Monad (unless, void, zipWithM_)
import Control.Monad.Except
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import Data.Map qualified as Map
import TC.Error
import TC.Types qualified as Tc
import Utils

tc :: Par.Prog -> Either TcError Tc.Prog
tc =
    runIdentity
        . runExceptT
        . flip evalStateT (Env mempty)
        . runTC
        . tcProg

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
infBlk = undefined

-- TODO: We need a state...
infStmt :: Par.Stmt -> TcM Tc.Stmt
infStmt = \case
    Par.Empty _ -> return Tc.Empty
    Par.BStmt pos (Par.Block bpos stmts) ->
        Tc.BStmt pos . Tc.Block bpos <$> mapM infStmt stmts
    Par.Decl pos typ items -> undefined
    Par.Ass pos ident expr -> undefined
    Par.Incr _ _ -> undefined
    Par.Decr _ _ -> undefined
    Par.Ret _ _ -> undefined
    Par.VRet _ -> undefined
    Par.Cond {} -> undefined
    Par.CondElse {} -> undefined
    Par.While {} -> undefined
    Par.SExp _ _ -> undefined

{- TODO: Type casting int to double is possible both ways I think, this should not be possible.
    And if it should be possible then the value needs to be floored.
-}
infExpr :: Par.Expr -> TcM Tc.Expr
infExpr = \case
    Par.EVar p i -> flip (Tc.EVar p) (convert i) <$> lookupVar p i
    Par.ELitInt p n -> return (Tc.ELit p int (Tc.LitInt n))
    Par.ELitDoub p n -> return (Tc.ELit p double (Tc.LitDouble n))
    Par.ELitTrue p -> return (Tc.ELit p double (Tc.LitBool True))
    Par.ELitFalse p -> return (Tc.ELit p double (Tc.LitBool False))
    Par.EString p str -> return (Tc.ELit p string (Tc.LitString str))
    Par.Neg pos expr -> do
        infexpr <- infExpr expr
        let typ = typeOf infexpr
        unless (isNumber typ) (throwError (TypeMismatch pos typ [int, double]))
        return (Tc.Neg pos typ infexpr)
    Par.Not pos expr -> do
        infexpr <- infExpr expr
        general <- typesMatch pos (typeOf infexpr) [bool]
        return (Tc.Neg pos general infexpr)
    Par.EMul p l op r -> do
        l' <- infExpr l
        r' <- infExpr r
        typleft <- typesMatch p (typeOf l') [double, int]
        typright <- typesMatch p (typeOf r') [double, int]
        typ <- unify p typleft typright
        return (Tc.EMul p typ l' (convert op) r')
    Par.EAdd p l op r -> do
        l' <- infExpr l
        r' <- infExpr r
        typleft <- typesMatch p (typeOf l') [double, int]
        typright <- typesMatch p (typeOf r') [double, int]
        typ <- unify p typleft typright
        return (Tc.EAdd p typ l' (convert op) r')
    Par.EAnd p l r -> do
        l' <- infExpr l
        r' <- infExpr r
        void $ typesMatch p (typeOf l') [bool]
        void $ typesMatch p (typeOf r') [bool]
        return (Tc.EAnd p l' r')
    Par.EOr p l r -> do
        l' <- infExpr l
        r' <- infExpr r
        void $ typesMatch p (typeOf l') [bool]
        void $ typesMatch p (typeOf r') [bool]
        return (Tc.EOr p l' r')
    Par.ERel p l op r -> do
        l' <- infExpr l
        r' <- infExpr r
        let equalityTypes = [string, bool, int, double]
        tl <- typesMatch p (typeOf l') equalityTypes
        tr <- typesMatch p (typeOf r') equalityTypes
        void $ unify p tl tr
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

newtype TcM a = TC {runTC :: StateT Env (Except TcError) a}
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

typesMatch :: (MonadError TcError m) => Tc.Position -> Tc.Type -> [Tc.Type] -> m Tc.Type
typesMatch p expected givens = do
    unless (expected `elem` givens) (throwError (TypeMismatch p expected givens))
    return expected

unify :: (MonadError TcError m) => Tc.Position -> Tc.Type -> Tc.Type -> m Tc.Type
unify pos l r
    | l == r = return l
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
