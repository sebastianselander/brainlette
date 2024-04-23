{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Braingen.Ir where

import BMM.Bmm qualified as B
import Braingen.LlvmAst
import Braingen.Output (OutputIr (outputIr))
import Control.Arrow ((>>>))
import Control.Monad (void)
import Control.Monad.State (State, get, gets, modify, put, runState)
import Data.DList hiding (map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack, takeWhile)
import Utils (concatFor, thow)
import Prelude hiding (takeWhile)
import Debug.Trace (traceShowM)

data Env = Env
    { instructions :: DList Stmt
    , labelCounter :: Integer
    , varCounter :: Int
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
braingenProg (B.Program tp) = do
    Ir <$> mapM braingenTopDef tp

braingenTopDef :: B.TopDef -> Either Text TopDef
braingenTopDef def = case def of
    B.StringGlobal name string -> pure $ ConstantString name string
    B.FnDef rt (B.Id i) a s -> do
        let ret = braingenType rt
        let args = map (appendArgName "arg" . braingenArg) a
        let argStmts = concatFor a argToStmts
        stmts <- braingenStmts s
        let stmts' = case ret of
                Void -> stmts ++ [RetVoid]
                _ -> stmts

        pure $ Define ret i args Nothing (argStmts <> stmts')
  where
    argToStmts = \case
        B.Argument t (B.Id n) ->
            [ Alloca (Variable n) (braingenType t)
            , Store
                ( appendArgName "arg" $
                    Argument (Just $ braingenType t) (Variable n)
                )
                (Variable n)
            ]

braingenStmts :: [B.Stmt] -> Either Text [Stmt]
braingenStmts =
    mapM_ (braingenStm Nothing)
        >>> flip runState (Env mempty 0 1)
        >>> \(_, e) -> Right $ toList (instructions e)

braingenStm :: Maybe Text -> B.Stmt -> BgM ()
braingenStm breakpoint stmt = case stmt of
    B.BStmt block -> mapM_ (braingenStm breakpoint) block
    B.Decl t (B.Id i) -> do
        output $ Alloca (Variable i) (braingenType t)
    B.Ass (B.Id a) expr@(t, _) -> do
        result <- braingenExpr expr
        output $ Store (Argument (pure $ braingenType t) result) (Variable a)
    B.Ret (Just expr@(t, _)) -> do
        result <- braingenExpr expr
        output $ Ret (Argument (pure $ braingenType t) result)
    B.Ret Nothing -> do
        output RetVoid
    B.CondElse expr s1 s2 -> do
        result <- braingenExpr expr
        lTrue <- getLabel "IfTrue"
        lFalse <- getLabel "IfFalse"
        lDone <- getLabel "IfDone"

        -- if
        output $ Br result lTrue lFalse
        -- if true
        output $ Label lTrue
        mapM_ (braingenStm (Just lDone)) s1
        output $ Jump lDone
        -- if false
        output $ Label lFalse
        mapM_ (braingenStm (Just lDone)) s2
        output $ Jump lDone
        -- if done
        output $ Label lDone
    B.Loop stmt -> do
        loopPoint <- getLabel "loop"
        breakpoint <- getLabel "breakpoint"
        output . Label $ loopPoint
        mapM_ (braingenStm (Just breakpoint)) stmt
        output . Jump $ loopPoint
        output . Label $ breakpoint
    B.SExp expr -> void $ braingenExpr expr
    B.Break -> do
        let bp = case breakpoint of
                Just bp -> bp
                Nothing -> error "break outside loop, report as INSERT BUG HERE :)"
        output . Jump $ bp

braingenExpr :: B.Expr -> BgM Variable
braingenExpr (ty, e) = case e of
    B.EGlobalVar (B.Id ident) -> do
        return (ConstVariable ident)
    B.EVar (B.Id ident) -> do
        var <- getTempVariable
        output $ Load var (braingenType ty) (Variable ident)
        return var
    B.ELit lit -> braingenLit lit
    B.Not e -> do
        exprVar <- braingenExpr e
        var <- getTempVariable
        output $ ICmp var Ieq I1 (Argument Nothing exprVar) (ConstArgument Nothing (LitInt 0))
        return var
    B.EApp (B.Id func) args -> do
        args <- mapM (\e@(t, _) -> Argument (Just $ braingenType t) <$> braingenExpr e) args
        case ty of
            B.Void -> do
                output $ VoidCall Nothing Nothing (braingenType ty) func args
                return (error "CODEGEN: tried referencing void variable")
            _ -> do
                result <- getTempVariable
                output $ Call result Nothing Nothing (braingenType ty) func args
                return result
    B.EAdd e1 op e2 -> do
        let t = braingenType ty
        r1 <- braingenExpr e1
        r2 <- braingenExpr e2
        let op' = braingenAddOp t op
        res <- getTempVariable
        output $ Arith res op' t (Argument Nothing r1) (Argument Nothing r2)
        pure res
    B.EMul e1 op e2 -> do
        let t = braingenType ty
        r1 <- braingenExpr e1
        r2 <- braingenExpr e2
        let op' = braingenMulOp t op
        res <- getTempVariable
        output $ Arith res op' t (Argument Nothing r1) (Argument Nothing r2)
        pure res
    B.Cast e@(t, _) -> do
        let ty' = braingenType ty
        let t' = braingenType t
        val <- braingenExpr e
        res <- getTempVariable
        output $ Cast (getCastOp ty' t') res t' val ty'
        pure res
    B.ERel (ty, l) op r -> do
        left <- braingenExpr (ty, l)
        right <- braingenExpr r
        case ty of
            B.Int -> do
                var <- getTempVariable
                output $ ICmp var (iRelOp op) I32 (Argument Nothing left) (Argument Nothing right)
                return var
            B.Double -> do
                var <- getTempVariable
                output $ FCmp var (fRelOp op) F64 (Argument Nothing left) (Argument Nothing right)
                return var
            ty -> error $ "TYPECHECK BUG: Relational comparison on invalid type: " <> show ty
    B.EAnd l r -> do
        l <- braingenExpr l
        r <- braingenExpr r
        var <- getTempVariable
        output $ And I1 (Argument Nothing l) (Argument Nothing r)
        return var
    B.EOr l r -> do
        l <- braingenExpr l
        r <- braingenExpr r
        var <- getTempVariable
        output $ Or I1 (Argument Nothing l) (Argument Nothing r)
        return var

iRelOp :: B.RelOp -> ICond
iRelOp = \case
    B.LTH -> Islt
    B.LE -> Isle
    B.GTH -> Isgt
    B.GE -> Isge
    B.EQU -> Ieq
    B.NE -> Ine

fRelOp :: B.RelOp -> FCond
fRelOp = \case
    B.LTH -> Folt
    B.LE -> Fole
    B.GTH -> Fogt
    B.GE -> Foge
    B.EQU -> Foeq
    B.NE -> Fone

braingenLit :: B.Lit -> BgM Variable
braingenLit lit = case lit of
    B.LitInt n -> do
        let ty = I32
        intermediate <- getTempVariable
        output $ Alloca intermediate ty
        output $ Store (ConstArgument (pure ty) (LitInt n)) intermediate
        var <- getTempVariable
        output $ Load var ty intermediate
        return var
    B.LitDouble n -> do
        let ty = F64
        intermediate <- getTempVariable
        output $ Alloca intermediate ty
        output $ Store (ConstArgument (pure ty) (LitDouble n)) intermediate
        var <- getTempVariable
        output $ Load var ty intermediate
        return var
    B.LitBool b -> do
        let ty = I1
        intermediate <- getTempVariable
        output $ Alloca intermediate ty
        output $ Store (ConstArgument (pure ty) (LitBool b)) intermediate
        var <- getTempVariable
        output $ Load var ty intermediate
        return var
    B.LitString _ -> error "CODEGEN BUG: String literal still exist"

----------------------------------- Helper functions -----------------------------------

-- | Push a statement onto the state
output :: Stmt -> BgM ()
output s = do
    state <- get
    let insts = instructions state
    put (state {instructions = insts `snoc` s})

-- | Return a label suffixed with a unique label to avoid label collisions.
getLabel :: Text -> BgM Text
getLabel t = do
    state <- get
    let current = labelCounter state
    put (state {labelCounter = current + 1})
    pure $ t <> "." <> thow current

{-| Return a temp variable, useful when calculating intermediate values.

For example: @int x = 1 + 2 + 3@ might generate:

@
%0 = add i32 1 2
@

@
%x = add i32 %0 3
@

And @%0@ can be obtained by calling @getTempVariable@.
-}
getTempVariable :: BgM Variable
getTempVariable = do
    v <- gets varCounter
    modify (\s -> s {varCounter = v + 1})
    return (Variable $ thow v)

-- | Convert a BMM type to an IR type
braingenType :: B.Type -> Type
braingenType = \case
    B.Int -> I32
    B.Boolean -> I1
    B.Double -> F64
    B.Void -> Void
    B.String -> Ptr
    B.TVar (B.Id x) -> CustomType x
    B.Fun t ts -> do
        let ret = braingenType t
        let args = map braingenType ts
        FunPtr ret args

-- | Append a text to argument name
appendArgName :: Text -> Argument -> Argument
appendArgName text = \case
    Argument t (Variable n) -> Argument t (Variable $ n <> "." <> text)
    x -> x

-- | Convert a BMM argument to an IR argument
braingenArg :: B.Arg -> Argument
braingenArg (B.Argument t (B.Id i)) = Argument (pure $ braingenType t) (Variable i)

-- | Convert a BMM add op to an IR Arithmetic instructions
braingenAddOp :: Type -> B.AddOp -> Arithmetic
braingenAddOp = \case
    I32 -> \case
        B.Plus -> Add
        B.Minus -> Sub
    F64 -> \case
        B.Plus -> FAdd
        B.Minus -> FSub
    _ -> error "error: report bug as a typeerror"

-- | Convert a BMM add op to an IR Arithmetic instructions
braingenMulOp :: Type -> B.MulOp -> Arithmetic
braingenMulOp = \case
    I32 -> \case
        B.Times -> Mul
        B.Div -> SDiv
        B.Mod -> URem
    F64 -> \case
        B.Times -> FMul
        B.Div -> FDiv
        B.Mod -> FRem
    _ -> error "error: report bug as a typeerror"

-- | Gets all constants
getConsts :: [B.TopDef] -> Set Text
getConsts td =
    Set.fromList $
        concatFor
            td
            ( \case
                B.StringGlobal n _ -> [n]
                _ -> []
            )

-- | Get cast op
getCastOp :: Type -> Type -> CastOp
getCastOp a b = case (a, b) of
    (F64, I32) -> FPtoSI
    (I32, F64) -> SItoFP
    _ -> Bitcast

consName :: (Show a) => a -> Text
consName = takeWhile (/= ' ') . thow

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
