{-# LANGUAGE LambdaCase #-}

module TC.Typechecker where

import Brainlette.Abs qualified as Par
import Control.Monad.Reader
import Data.Map (Map)
import Data.Map qualified as Map
import TC.Types qualified as Tc
import Utils

tc :: Par.Prog -> Either String Tc.Prog
tc = TODO

tcExpr :: Par.Expr -> Tc.Expr
tcExpr = TODO

newtype Env = Env {runEnv :: Map Par.Ident (Tc.Type, [Tc.Type])}
    deriving (Show, Eq, Ord)

newtype TC a = TC {runTC :: Reader Env a}
    deriving (Functor, Applicative, Monad, MonadReader Env)

-- | Extract the types from all top level definitions '⌣'
getDefs :: Par.Prog -> Map Par.Ident (Tc.Type, [Tc.Type])
getDefs (Par.Program prog) =
    let argTypes args = for args (\(Par.Argument t _) -> t)
     in Map.fromList . for prog $ \(Par.FnDef rt ident args _) ->
            (ident, (convert rt, convert . argTypes $ args))

{-| Type class to help converting from the old parser types
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