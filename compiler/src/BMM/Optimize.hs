{-# LANGUAGE LambdaCase #-}
module BMM.Optimize where

import BMM.Bmm
import Data.Fixed (mod')

optimizeExpr :: Expr -> Maybe Expr
optimizeExpr = \case
    (ty, EVar id) -> Nothing
    (ty, ELit lit) -> case (ty, lit) of
        (Double, LitInt n) -> Just (Double, ELit $ LitDouble (fromInteger n))
        (Int, LitDouble n) -> Just (Int, ELit $ LitInt (truncate n)) -- Should probably never happen
    (ty, EApp id exprs) -> Nothing
    (ty, EString str) -> Nothing
    (ty, Not expr) -> do
        e <- optimizeExpr expr
        case e of
            (ty, ELit (LitBool b)) -> Just (ty, ELit $ LitBool (not b))
            _ -> Nothing
    (ty, EMul l op r) -> do
        l <- optimizeExpr l
        r <- optimizeExpr r
        case (l, r) of
            ((_, ELit n), (_, ELit m)) ->
                case (n, m, op) of
                    (LitInt n, LitInt m, Times) -> Just (ty, ELit (LitInt (n * m)))
                    (LitInt n, LitInt m, Div) -> Just (ty, ELit (LitInt (n `div` m)))
                    (LitInt n, LitInt m, Mod) -> Just (ty, ELit (LitInt (n `mod` m)))

                    (LitDouble n, LitInt m, Times) -> Just (ty, ELit (LitDouble (n * fromInteger m)))
                    (LitDouble n, LitInt m, Div) -> Just (ty, ELit (LitDouble (n / fromInteger m)))
                    (LitDouble n, LitInt m, Mod) -> Just (ty, ELit (LitDouble (n `mod'` fromInteger m)))

                    (LitInt n, LitDouble m, Times) -> Just (ty, ELit (LitDouble (fromInteger n * m)))
                    (LitInt n, LitDouble m, Div) -> Just (ty, ELit (LitDouble (fromInteger n / m)))
                    (LitInt n, LitDouble m, Mod) -> Just (ty, ELit (LitDouble (fromInteger n `mod'` m)))

                    (LitDouble n, LitDouble m, Times) -> Just (ty, ELit (LitDouble (n * m)))
                    (LitDouble n, LitDouble m, Div) -> Just (ty, ELit (LitDouble (n / m)))
                    (LitDouble n, LitDouble m, Mod) -> Just (ty, ELit (LitDouble (n `mod'` m)))
                    _ -> error "Brainlette bug: please report as multiplication optimize"
            _ -> Nothing
    (ty, EAdd l op r) -> do
        l <- optimizeExpr l
        r <- optimizeExpr r
        case (l, r) of
            ((_, ELit n), (_, ELit m)) ->
                case (n, m, op) of
                    (LitInt n, LitInt m, Plus) -> Just (ty, ELit (LitInt (n + m)))
                    (LitInt n, LitInt m, Minus) -> Just (ty, ELit (LitInt (n - m)))

                    (LitDouble n, LitInt m, Plus) -> Just (ty, ELit (LitDouble (n + fromInteger m)))
                    (LitDouble n, LitInt m, Minus) -> Just (ty, ELit (LitDouble (n - fromInteger m)))

                    (LitInt n, LitDouble m, Plus) -> Just (ty, ELit (LitDouble (fromInteger n + m)))
                    (LitInt n, LitDouble m, Minus) -> Just (ty, ELit (LitDouble (fromInteger n - m)))

                    (LitDouble n, LitDouble m, Plus) -> Just (ty, ELit (LitDouble (n + m)))
                    (LitDouble n, LitDouble m, Minus) -> Just (ty, ELit (LitDouble (n - m)))
                    _ -> error "Brainlette bug: please report as addition optimize"
            _ -> Nothing
    (ty, ERel l op r) -> undefined
    (ty, EAnd l r) -> undefined
    (ty, EOr l r) -> undefined
    (ty, Cast expr) -> Nothing
