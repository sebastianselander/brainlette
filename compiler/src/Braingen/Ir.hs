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
braingen :: B.Prog a -> Either String Ir
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

braingenProg :: B.Prog a -> BgM Ir
braingenProg (B.Program tp) = Ir <$> mapM braingenTopDef tp

braingenType :: B.Type a -> BgM Type
braingenType t = case t of
    B.TVar (B.Id _ "int") -> pure I32
    B.TVar (B.Id _ x) -> pure $ CustomType x
    B.Fun t ts -> do
        ret <- braingenType t
        args <- mapM braingenType ts
        pure $ FunPtr ret args

braingenArg :: B.Arg a -> BgM Argument
braingenArg (B.Argument _ t (B.Id _ i)) = flip Argument i <$> braingenType t

braingenStm :: B.Stmt a -> BgM [Stmt]
braingenStm = \case
    B.BStmt _ block -> pure . pure . Comment $ "TODO block"
    B.Decl _ t (B.Id _ i) -> pure . pure . Comment $ "TODO decl"
    B.Ass _ (B.Id _ a) expr -> pure . pure . Comment $ "TODO assign"
    B.Ret _ expr -> pure . pure . Comment $ "TODO return"
    B.CondElse _ cexpr a'stmt b'stmt -> pure . pure . Comment $ "TODO cond else"
    B.Loop _ stmt -> pure . pure . Comment $ "TODO loop"
    B.SExp _ expr -> pure . pure . Comment $ "TODO sexp"

braingenTopDef :: B.TopDef a -> BgM TopDef
braingenTopDef (B.FnDef ret (B.Id _ i) a s) = do
    ret <- braingenType ret
    args <- mapM braingenArg a
    stmts <- concat <$> mapM braingenStm s
    pure $ Define ret i args NoAttribute stmts