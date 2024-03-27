{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Braingen.Ir where

import BMM.Bmm qualified as B
import Braingen.LlvmAst
import Control.Arrow ((>>>))
import Control.Monad.Except (Except, MonadError, runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State (StateT)
import Control.Monad.State.Lazy (MonadState, evalStateT)

data Env = Env {}
    deriving (Show, Eq, Ord)

data Ctx = Ctx {}
    deriving (Show, Eq, Ord)

data BraingenError
    deriving (Show)

newtype BgM a = BG {runBgM :: StateT Env (ReaderT Ctx (Except BraingenError)) a}
    deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadState Env, MonadError BraingenError)

-- | Pump those wrinkles 🧠
braingen :: B.Prog a -> Either String (Ir a)
braingen =
    braingenProg
        >>> runBgM
        >>> flip evalStateT Env
        >>> flip runReaderT Ctx
        >>> runExceptT
        >>> runIdentity
        >>> \case
            Left err -> Left $ show err
            Right p -> Right p

braingenProg :: B.Prog a -> BgM (Ir a)
braingenProg (B.Program tp) = Ir <$> mapM braingenTopDef tp

braingenType :: B.Type a -> BgM Type
braingenType t = pure $ case t of
    B.TVar (B.Id _ "int") -> I32
    B.Fun t ts -> undefined

braingenTopDef :: B.TopDef a -> BgM (TopDef a)
braingenTopDef (B.FnDef t i a s) = do
    t <- braingenType t
    undefined