{-# LANGUAGE LambdaCase #-}

module TC.Typechecker where

import Brainlette.Abs qualified as Par
import Control.Monad (unless, void, zipWithM_)
import Control.Monad.Except
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import Data.Map qualified as Map
import TC.Error
import TC.Types qualified as Tc
import Utils
import Control.Monad.State (StateT, MonadState, evalStateT, gets)

tc :: Par.Prog -> Either TcError Tc.Prog
tc =
    runIdentity
        . runExceptT
        . flip evalStateT (Env mempty)
        . runTC
        . tcProg

tcProg :: Par.Prog -> Check Tc.Prog
tcProg (Par.Program _ topdefs) = Tc.Program <$> mapM infDef topdefs

infDef :: Par.TopDef -> Check Tc.TopDef
infDef (Par.FnDef p o'rt name o'args (Par.Block bp o'block)) = do
    let rt = convert o'rt :: Tc.Type
    let args = for o'args $ \(Par.Argument p t i) ->
            Tc.Argument p (convert t) (convert i)
    block <- TODO
    pure $ Tc.FnDef p rt (convert name) args (Tc.Block bp block)

-- TODO: We need a state...
infStmt :: Par.Stmt -> Check Tc.Stmt
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
infExpr :: Par.Expr -> Check Tc.Expr
infExpr = \case
    Par.EVar p i -> flip (Tc.EVar p) (convert i) <$> lookupVar p i
    Par.ELitInt p n -> return (Tc.ELit p Tc.Int (Tc.LitInt n))
    Par.ELitDoub p n -> return (Tc.ELit p Tc.Double (Tc.LitDouble n))
    Par.ELitTrue p -> return (Tc.ELit p Tc.Double (Tc.LitBool True))
    Par.ELitFalse p -> return (Tc.ELit p Tc.Double (Tc.LitBool False))
    Par.EString p str -> return (Tc.ELit p Tc.String (Tc.LitString str))
    Par.Neg pos expr -> do
        infexpr <- infExpr expr
        general <- typesMatch pos (typeOf infexpr) isNumber
        return (Tc.Neg pos general infexpr)
    Par.Not pos expr -> do
        infexpr <- infExpr expr
        general <- typesMatch pos (typeOf infexpr) [Tc.Bool]
        return (Tc.Neg pos general infexpr)
    Par.EMul p l op r -> do
        l' <- infExpr l
        r' <- infExpr r
        typleft <- typesMatch p (typeOf l') [Tc.Double, Tc.Int]
        typright <- typesMatch p (typeOf r') [Tc.Double, Tc.Int]
        typ <- unify p typleft typright
        return (Tc.EMul p typ l' (convert op) r')
    Par.EAdd p l op r -> do
        l' <- infExpr l
        r' <- infExpr r
        typleft <- typesMatch p (typeOf l') [Tc.Double, Tc.Int]
        typright <- typesMatch p (typeOf r') [Tc.Double, Tc.Int]
        typ <- unify p typleft typright
        return (Tc.EAdd p typ l' (convert op) r')
    Par.EAnd p l r -> do
        l' <- infExpr l
        r' <- infExpr r
        void $ typesMatch p (typeOf l') [Tc.Bool]
        void $ typesMatch p (typeOf r') [Tc.Bool]
        return (Tc.EAnd p l' r')
    Par.EOr p l r -> do
        l' <- infExpr l
        r' <- infExpr r
        void $ typesMatch p (typeOf l') [Tc.Bool]
        void $ typesMatch p (typeOf r') [Tc.Bool]
        return (Tc.EOr p l' r')
    Par.ERel p l op r -> do
        l' <- infExpr l
        r' <- infExpr r
        let equalityTypes = [Tc.String, Tc.Bool, Tc.Int, Tc.Double]
        tl <- typesMatch p (typeOf l') equalityTypes
        tr <- typesMatch p (typeOf r') equalityTypes
        void $ unify p tl tr
        return (Tc.ERel p l' (convert op) r')
    Par.EApp p ident exprs -> do
        (rt, argTypes) <- lookupFn p ident
        exprs' <- mapM infExpr exprs
        zipWithM_ (unify p) (fmap typeOf exprs') argTypes
        return (Tc.EApp p rt (convert ident) exprs')

newtype Env = Env {variables :: Map Tc.Ident (Tc.Type, [Tc.Type])}
    deriving (Show, Eq, Ord)

newtype Check a = TC {runTC :: StateT Env (Except TcError) a}
    deriving (Functor, Applicative, Monad, MonadState Env, MonadError TcError)

-- | Extract the types from all top level definitions '⌣'
getDefs :: Par.Prog -> Map Par.Ident (Tc.Type, [Tc.Type])
getDefs (Par.Program _ prog) =
    let argTypes = map (\(Par.Argument _ t _) -> t)
     in Map.fromList . for prog $ \(Par.FnDef _ rt ident args _) ->
            (ident, (convert rt, convert . argTypes $ args))

lookupVar :: Tc.Position -> Par.Ident -> Check Tc.Type
lookupVar p i = do
    typ <- gets (Map.lookup (convert i) . variables)
    case typ of
        Just (rt, []) -> return rt
        _ -> throwError (UnboundVariable p (convert i))

lookupFn :: Tc.Position -> Par.Ident -> Check (Tc.Type, [Tc.Type])
lookupFn p i = do
    typ <- gets (Map.lookup (convert i) . variables)
    case typ of
        Just t -> return t
        _ -> throwError (UnboundVariable p (convert i))

{-| Type class to help converting from the parser types
  to the type checker type
-}
class Convert a b where
    convert :: a -> b

instance (Convert a b) => Convert [a] [b] where
    convert = map convert

instance Convert Par.Type Tc.Type where
    convert = \case
        Par.Int _ -> Tc.Int
        Par.Doub _ -> Tc.Double
        Par.Bool _ -> Tc.Bool
        Par.Void _ -> Tc.Void
        Par.Fun _ rt args -> Tc.Fun (convert rt) (convert args)

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

class TypeOf a where
    typeOf :: a -> Tc.Type

instance TypeOf Tc.Expr where
    typeOf = \case
        Tc.EVar _ t _ -> t
        Tc.ELit _ t _ -> t
        Tc.Neg _ t _ -> t
        Tc.Not _ _ -> Tc.Bool
        Tc.EApp _ rt _ _ -> rt
        Tc.EMul _ t _ _ _ -> t
        Tc.EAdd _ t _ _ _ -> t
        Tc.ERel {} -> Tc.Bool
        Tc.EAnd {} -> Tc.Bool
        Tc.EOr {} -> Tc.Bool

typesMatch :: (MonadError TcError m) => Tc.Position -> Tc.Type -> [Tc.Type] -> m Tc.Type
typesMatch p expected givens = do
    unless (expected `elem` givens) (throwError (TypeMismatch p expected givens))
    return expected

unify :: (MonadError TcError m) => Tc.Position -> Tc.Type -> Tc.Type -> m Tc.Type
unify pos l r
    | l == r = return l
    | otherwise = case (l, r) of
        (Tc.Int, Tc.Double) -> return Tc.Double
        (Tc.Double, Tc.Int) -> return Tc.Double
        _ -> throwError (TypeMismatch pos l [r])

isNumber :: [Tc.Type]
isNumber = [Tc.Double, Tc.Int]
