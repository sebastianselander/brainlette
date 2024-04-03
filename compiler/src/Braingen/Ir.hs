{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Braingen.Ir where

import BMM.Bmm qualified as B
import Braingen.LlvmAst
import Braingen.Output (OutputIr (outputIr))
import Control.Arrow ((>>>))
import Control.Monad.State (MonadState (get, put), State, runState)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Utils (thow)

data Env = Env
    { instructions :: [Stmt]
    , labelCounter :: Integer
    , tempVariables :: Map Text Integer
    }
    deriving (Show)

data BraingenError
    deriving (Show)

type BgM = State Env

-- | Pump those wrinkles 🧠
braingen :: B.Prog -> Either Text Text
braingen =
    braingenProg >>> \case
        Left err -> Left . pack $ "The impossible happened: " <> show err
        Right p -> Right $ outputIr p

braingenProg :: B.Prog -> Either Text Ir
braingenProg (B.Program tp) = Ir <$> mapM braingenTopDef tp

braingenTopDef :: B.TopDef -> Either Text TopDef
braingenTopDef (B.FnDef rt (B.Id i) a s) = do
    let ret = braingenType rt
    let args = map braingenArg a
    stmts <- braingenStmts s
    pure $ Define ret i args NoAttribute stmts

braingenStmts :: [B.Stmt] -> Either Text [Stmt]
braingenStmts =
    mapM_ braingenStm
        >>> flip runState (Env [] 0 Map.empty)
        >>> \(_, e) -> Right $ instructions e

braingenStm :: B.Stmt -> BgM ()
braingenStm = \case
    B.BStmt block -> do
        output . Comment $ "TODO     BLOCK: " <> thow block
    B.Decl t (B.Id i) -> do
        output . Comment $ "TODO      DECL: " <> i <> " := " <> thow t
    B.Ass (B.Id a) expr -> do
        output . Comment $ "TODO    ASSIGN: " <> a <> " = " <> thow expr
    B.Ret (Just expr) -> do
        output . Comment $ "TODO    RETURN: " <> thow expr
    B.Ret Nothing -> do
        output RetVoid
    B.CondElse cexpr s1 s2 -> do
        output . Comment $ "TODO COND ELSE: if " <> thow cexpr <> " ? " <> thow s1 <> " : " <> thow s2
    B.Loop stmt -> do
        output . Comment $ "TODO      LOOP: " <> thow stmt
    B.SExp expr -> do
        output . Comment $ "TODO      EXPR: " <> thow expr
    B.Break -> do
        output . Comment $ "TODO     BREAK"

braingenExpr :: B.Expr -> BgM (Text, [Stmt])
braingenExpr = pure . pure ("TODO", [Comment "TODO Expr"])

----------------------------------- Helper functions -----------------------------------

-- | Push a statement onto the state
output :: Stmt -> BgM ()
output s = do
    state <- get
    let insts = instructions state
    put (state {instructions = insts ++ [s]})

-- | Return a label suffixed with a unique label to avoid label collisions.
getLabel :: Text -> BgM Text
getLabel t = do
    state <- get
    let current = labelCounter state
    put (state {labelCounter = current + 1})
    pure $ t <> "." <> (thow current)

{-| Return a temp variable, useful when calculating intermediate values.

For example: @int x = 1 + 2 + 3@ might generate:

@
%x.0 = add i32 1 2
@

@
%x = add i32 %x.0 3
@

And @x.0@ can be obtained by calling @getTempVariable "x"@.
-}
getTempVariable :: Text -> BgM Text
getTempVariable t = do
    state <- get
    let vars = tempVariables state
    case Map.lookup t vars of
        Just val -> do
            let vars' = Map.insert t (val + 1) vars
            put (state {tempVariables = vars'})
            pure $ t <> "." <> (thow val)
        Nothing -> do
            let vars' = Map.insert t 0 vars
            put (state {tempVariables = vars'})
            pure $ t <> ".0"

-- | Convert a BMM type to an IR type
braingenType :: B.Type -> Type
braingenType t = case t of
    B.Int -> I32
    B.Boolean -> I1
    B.Double -> F64
    B.Void -> error "TODO: braingen type void"
    B.TVar (B.Id x) -> CustomType x
    B.Fun t ts -> do
        let ret = braingenType t
        let args = map braingenType ts
        FunPtr ret args

-- | Convert a BMM argument to an IR argument
braingenArg :: B.Arg -> Argument
braingenArg (B.Argument t (B.Id i)) = Argument (braingenType t) i

----------------------------------- Test cases -----------------------------------
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
