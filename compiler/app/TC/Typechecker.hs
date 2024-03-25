{-# LANGUAGE LambdaCase #-}

module TC.Typechecker where

import Brainlette.Abs qualified as Par
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import Data.Map qualified as Map
import TC.Error
import TC.Types qualified as Tc
import Utils

tc :: Par.Prog -> Either String Tc.Prog
tc = TODO

infExpr :: Par.Expr -> Check Tc.Expr
infExpr = \case
    Par.EVar p i -> flip Tc.EVar (convert i) <$> lookupVar p i
    Par.ELitInt _ n -> return (Tc.ELit Tc.Int (Tc.LitInt n))
    Par.ELitDoub _ n -> return (Tc.ELit Tc.Double (Tc.LitDouble n))
    Par.ELitTrue _ -> return (Tc.ELit Tc.Double (Tc.LitBool True))
    Par.ELitFalse _ -> return (Tc.ELit Tc.Double (Tc.LitBool False))
    Par.EString _ str -> return (Tc.ELit Tc.String (Tc.LitString str))
    Par.Neg _ expr -> TODO
    Par.Not _ expr -> TODO
    Par.EApp _ ident exprs -> TODO
    Par.EMul {} -> TODO
    Par.EAdd {} -> TODO
    Par.ERel {} -> TODO
    Par.EAnd _ _ _ -> TODO
    Par.EOr _ _ _ -> TODO

tcExpr :: Tc.Type -> Par.Expr -> Check ()
tcExpr typ expr = case expr of
    Par.EVar p _ -> infExpr expr >>= typesMatch p typ . (: []) . typeOf
    Par.ELitInt p _ -> typesMatch p typ [Tc.Int, Tc.Double]
    Par.ELitDoub p _ -> typesMatch p typ [Tc.Double]
    Par.ELitTrue p -> typesMatch p typ [Tc.Bool]
    Par.ELitFalse p -> typesMatch p typ [Tc.Bool]
    Par.EString p _ -> typesMatch p typ [Tc.String]
    Par.Neg _ expr -> tcExpr typ expr
    Par.Not _ expr -> tcExpr Tc.Bool expr
    Par.EApp _ ident exprs -> TODO
    Par.EMul _ l _ r -> TODO
    Par.EAdd {} -> TODO
    Par.ERel {} -> TODO
    Par.EAnd _ _ _ -> TODO
    Par.EOr _ _ _ -> TODO

newtype Env = Env {variables :: Map Par.Ident (Tc.Type, [Tc.Type])}
    deriving (Show, Eq, Ord)

newtype Check a = TC {runTC :: ReaderT Env (Except TcError) a}
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadError TcError)

-- | Extract the types from all top level definitions '⌣'
getDefs :: Par.Prog -> Map Par.Ident (Tc.Type, [Tc.Type])
getDefs (Par.Program p prog) =
    let argTypes = map (\(Par.Argument _ t _) -> t)
     in Map.fromList . for prog $ \(Par.FnDef _ rt ident args _) ->
            (ident, (convert rt, convert . argTypes $ args))

lookupVar :: Position -> Par.Ident -> Check Tc.Type
lookupVar p i = do
    typ <- asks (Map.lookup i . variables)
    case typ of
        Just (rt, []) -> return rt
        _ -> throwError (UnboundVariable p (convert i))

lookupFn :: Position -> Par.Ident -> Check (Tc.Type, [Tc.Type])
lookupFn p i = do
    typ <- asks (Map.lookup i . variables)
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

typesMatch :: (MonadError TcError m) => Position -> Tc.Type -> [Tc.Type] -> m ()
typesMatch p expected givens =
    unless (expected `elem` givens) (throwError (TypeMismatch p expected givens))
