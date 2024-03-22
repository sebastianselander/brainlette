module TC.Typechecker where

import Brainlette.Abs qualified as Par
import TC.Types qualified as Tc
import Utils

tc :: Par.Prog -> Either String Tc.Prog
tc = TODO

tcExpr :: Par.Expr -> Tc.Expr
tcExpr = TODO
