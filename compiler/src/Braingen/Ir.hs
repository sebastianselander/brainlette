{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Braingen.Ir where

import BMM.Bmm qualified as B
import Braingen.LlvmAst
import Braingen.Output (OutputIr (outputIr))
import Control.Arrow ((>>>))
import Control.Monad.Except (Except, MonadError, runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State (StateT)
import Control.Monad.State.Lazy (MonadState, evalStateT)
import Data.Text (Text, pack)

data Env = Env {}
    deriving (Show, Eq, Ord)

data Ctx = Ctx {}
    deriving (Show, Eq, Ord)

data BraingenError
    deriving (Show)

newtype BgM a = BG {runBgM :: StateT Env (ReaderT Ctx (Except BraingenError)) a}
    deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadState Env, MonadError BraingenError)

-- | Pump those wrinkles 🧠
braingen :: B.Prog -> Either Text Text
braingen =
    braingenProg
        >>> runBgM
        >>> flip evalStateT Env
        >>> flip runReaderT Ctx
        >>> runExceptT
        >>> runIdentity
        >>> \case
            Left err -> Left . pack . show $ err
            Right p -> Right $ outputIr p

braingenProg :: B.Prog -> BgM Ir
braingenProg (B.Program tp) = Ir <$> mapM braingenTopDef tp

braingenType :: B.Type -> BgM Type
braingenType t = case t of
    B.Int -> pure I32
    B.Boolean -> pure I1
    B.Double -> pure F64
    B.Void -> error "TODO: braingen type void"
    B.TVar (B.Id x) -> pure $ CustomType x
    B.Fun t ts -> do
        ret <- braingenType t
        args <- mapM braingenType ts
        pure $ FunPtr ret args

braingenArg :: B.Arg -> BgM Argument
braingenArg (B.Argument t (B.Id i)) = flip Argument i <$> braingenType t

braingenStm :: B.Stmt -> BgM [Stmt]
braingenStm = \case
    B.BStmt block -> pure . pure . Comment $ "TODO block"
    B.Decl t (B.Id i) -> pure . pure . Comment $ "TODO decl"
    B.Ass (B.Id a) expr -> pure . pure . Comment $ "TODO assign"
    B.Ret expr -> pure . pure . Comment $ "TODO return"
    B.CondElse cexpr a'stmt b'stmt -> pure . pure . Comment $ "TODO cond else"
    B.Loop stmt -> pure . pure . Comment $ "TODO loop"
    B.SExp expr -> pure . pure . Comment $ "TODO sexp"
    B.Break -> pure . pure . Comment $ "TODO break"

braingenTopDef :: B.TopDef -> BgM TopDef
braingenTopDef (B.FnDef ret (B.Id i) a s) = do
    ret <- braingenType ret
    args <- mapM braingenArg a
    stmts <- concat <$> mapM braingenStm s
    pure $ Define ret i args NoAttribute stmts

braingenExpr :: B.Expr -> BgM Stmt
braingenExpr = undefined

testProg :: B.Prog
testProg =
    B.Program
        [ B.FnDef
            B.Int
            (B.Id "add")
            [ B.Argument B.Int (B.Id "x")
            , B.Argument B.Int (B.Id "y")
            ]
            [ B.Ret $
                Just
                    ( B.TVar $ B.Id "Int"
                    , B.EAdd
                        (B.TVar $ B.Id "Int", B.ELit $ B.LitInt 123)
                        B.Plus
                        (B.TVar $ B.Id "Int", B.ELit $ B.LitInt 123)
                    )
            ]
        ]
