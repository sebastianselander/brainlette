{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Braingen.TH where

import Control.Monad.Extra (concatMapM)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

gen :: String -> Q [Dec]
gen str = do
    name' <- lookupTypeName str
    name <- maybe (error "Data type not found") return name'
    reify name >>= \case
        TyConI (DataD _ _ _ _ cons _) -> do
            concatMapM genCon cons
        _ -> error "Not a type constructor"

genCon :: Con -> Q [Dec]
genCon con@(NormalC nm _) = do
    ty <- reifyType nm
    let ty' = delinearize $ bgmLast ty
    let args = nArgs ty'
        name = getName con
        pats = map (VarP . mkName . return) $ take args ['a' ..]
        vars = map (VarE . mkName . return) $ take args ['a' ..]
        exp =
            InfixE
                (Just $ VarE (mkName "output"))
                (VarE $ mkName "$")
                (Just $ foldl AppE (ConE (mkName name)) vars)
    return
        [ SigD (mkName (small name)) ty'
        , FunD (mkName (small name)) [Clause pats (NormalB exp) []]
        ]
genCon _ = error "Not a normal constructor"

mkFun :: String -> Q [Dec]
mkFun nm = [d|$name = output $ $(mkCon nm)|]
  where
    name = varP $ mkName (small nm)

mkCon :: String -> Q Exp
mkCon str = conE (mkName str)

small :: String -> String
small [] = []
small (x : xs) = toLower x : xs

getName :: Con -> String
getName (NormalC nm _) = case nm of
    Name occNm _ -> coerce occNm
getName _ = undefined

debug :: (Show a) => a -> b
debug = error . show

bgmLast :: Type -> Type
bgmLast = \case
    AppT t1 t2 -> AppT t1 (bgmLast t2)
    ConT {} -> (AppT (ConT . mkName $ "Braingen.Ir.BgM") (TupleT 0))
    x -> delinearize x

delinearize :: Type -> Type
delinearize = \case
    AppT MulArrowT _ -> ArrowT
    AppT t1 t2 -> AppT (delinearize t1) (delinearize t2)
    ty -> ty

nArgs :: (Num a) => Type -> a
nArgs = \case
    AppT MulArrowT ty -> 1 + nArgs ty
    AppT ArrowT ty -> 1 + nArgs ty
    AppT l r -> nArgs l + nArgs r
    ForallT {} -> 0
    ForallVisT {} -> 0
    AppKindT {} -> 0
    SigT {} -> 0
    VarT _ -> 0
    ConT _ -> 0
    PromotedT {} -> 0
    InfixT {} -> 0
    UInfixT {} -> 0
    PromotedInfixT {} -> 0
    PromotedUInfixT {} -> 0
    ParensT _ -> 0
    TupleT _ -> 0
    UnboxedTupleT _ -> 0
    UnboxedSumT _ -> 0
    ArrowT -> 0
    MulArrowT -> 0
    EqualityT -> 0
    ListT -> 0
    PromotedTupleT _ -> 0
    PromotedNilT -> 0
    PromotedConsT -> 0
    StarT -> 0
    ConstraintT -> 0
    LitT _ -> 0
    WildCardT -> 0
    ImplicitParamT {} -> 0
