{-# LANGUAGE LambdaCase #-}

module TC.Typechecker where

import Brainlette.Abs qualified as Par
import Data.Map (Map)
import Data.Map qualified as Map
import TC.Types qualified as Tc
import Utils

tc :: Par.Prog -> Either String Tc.Prog
tc = TODO

tcExpr :: Par.Expr -> Tc.Expr
tcExpr = TODO
