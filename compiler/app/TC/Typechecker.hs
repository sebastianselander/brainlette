{-# LANGUAGE LambdaCase #-}

module TC.Typechecker where

import Brainlette.Abs qualified as Par
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import Data.Map qualified as Map
import TC.Error
import TC.Types qualified as Tc
import Utils
import Control.Monad (unless)

tc :: Par.Prog -> Either String Tc.Prog
tc = TODO

infExpr :: Par.Expr -> Check Tc.Expr
infExpr = \case
    Par.EVar i -> flip Tc.EVar (convert i) <$> lookupVar i
    Par.ELitInt n -> return (Tc.ELit Tc.Int (Tc.LitInt n))
    Par.ELitDoub n -> return (Tc.ELit Tc.Double (Tc.LitDouble n))
    Par.ELitTrue -> return (Tc.ELit Tc.Double (Tc.LitBool True))
    Par.ELitFalse -> return (Tc.ELit Tc.Double (Tc.LitBool False))
    Par.EString str -> return (Tc.ELit Tc.String (Tc.LitString str))
    Par.Neg expr -> TODO
    Par.Not expr -> TODO
    Par.EApp ident exprs -> TODO
    Par.EMul {} -> TODO
    Par.EAdd {} -> TODO
    Par.ERel {} -> TODO
    Par.EAnd _ _ -> TODO
    Par.EOr _ _ -> TODO

tcExpr :: Tc.Type -> Par.Expr -> Check ()
tcExpr typ expr = case expr of
    Par.EVar _ -> infExpr expr >>= typesMatch typ . (:[]) . typeOf
    Par.ELitInt _ -> typesMatch typ [Tc.Int, Tc.Double]
    Par.ELitDoub _ -> typesMatch typ [Tc.Double]
    Par.ELitTrue -> typesMatch typ [Tc.Bool]
    Par.ELitFalse -> typesMatch typ [Tc.Bool]
    Par.EString _ -> typesMatch typ [Tc.String]
    Par.Neg expr -> tcExpr typ expr
    Par.Not expr -> tcExpr Tc.Bool expr
    Par.EApp ident exprs -> TODO
    Par.EMul l _ r -> TODO
    Par.EAdd {} -> TODO
    Par.ERel {} -> TODO
    Par.EAnd _ _ -> TODO
    Par.EOr _ _ -> TODO

newtype Env = Env {variables :: Map Par.Ident (Tc.Type, [Tc.Type])}
    deriving (Show, Eq, Ord)

newtype Check a = TC {runTC :: ReaderT Env (Except TcError) a}
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadError TcError)

-- | Extract the types from all top level definitions '⌣'
getDefs :: Par.Prog -> Map Par.Ident (Tc.Type, [Tc.Type])
getDefs (Par.Program prog) =
    let argTypes = map (\(Par.Argument t _) -> t)
     in Map.fromList . for prog $ \(Par.FnDef rt ident args _) ->
            (ident, (convert rt, convert . argTypes $ args))

lookupVar :: Par.Ident -> Check Tc.Type
lookupVar i = do
    typ <- asks (Map.lookup i . variables)
    case typ of
        Just (rt, []) -> return rt
        _ -> throwError (UnboundVariable (convert i))

lookupFn :: Par.Ident -> Check (Tc.Type, [Tc.Type])
lookupFn i = do
    typ <- asks (Map.lookup i . variables)
    case typ of
        Just t -> return t
        _ -> throwError (UnboundVariable (convert i))

{-| Type class to help converting from the parser types
  to the type checker type
-}
class Convert a b where
    convert :: a -> b

instance (Convert a b) => Convert [a] [b] where
    convert = map convert

instance Convert Par.Type Tc.Type where
    convert = \case
        Par.Int -> Tc.Int
        Par.Doub -> Tc.Double
        Par.Bool -> Tc.Bool
        Par.Void -> Tc.Void
        Par.Fun rt args -> Tc.Fun (convert rt) (convert args)

instance Convert Par.Ident Tc.Ident where
    convert (Par.Ident s) = Tc.Ident s

class TypeOf a where
    typeOf :: a -> Tc.Type

instance TypeOf Tc.Expr where
  typeOf = \case
        Tc.EVar t _ -> t
        Tc.ELit t _ -> t
        Tc.Neg t _ -> t
        Tc.Not _ -> Tc.Bool
        Tc.EApp rt _ _ -> rt
        Tc.EMul t _ _ _ -> t
        Tc.EAdd t _ _ _ -> t
        Tc.ERel {} -> Tc.Bool
        Tc.EAnd _ _ -> Tc.Bool
        Tc.EOr _ _ -> Tc.Bool

typesMatch :: MonadError TcError m => Tc.Type -> [Tc.Type] -> m ()
typesMatch expected givens = 
    unless (expected `elem` givens) (throwError (TypeMismatch expected givens))
