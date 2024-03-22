{-# LANGUAGE LambdaCase #-}

module TC.Typechecker where

import Brainlette.Abs qualified as Par
import Data.Map (Map)
import Data.Map qualified as Map
import TC.Types qualified as Tc
import Utils
import Control.Monad.Reader

tc :: Par.Prog -> Either String Tc.Prog
tc = TODO

tcExpr :: Par.Expr -> Tc.Expr
tcExpr = TODO

newtype Env = Env { runEnv :: Map Par.Ident (Tc.Type, [Tc.Type]) }
    deriving (Show, Eq, Ord)

newtype TC a = TC { runTC :: Reader Env a }
    deriving (Functor, Applicative, Monad, MonadReader Env)
