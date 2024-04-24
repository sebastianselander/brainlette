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
import Data.Text (Text, pack, toTitle)
import Utils (concatFor, thow)
import Prelude hiding (takeWhile)

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
        let stmts' = stmts ++ 
                case rt of
                    B.Void -> [RetVoid, Unreachable]
                    _ -> [Unreachable]

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
        >>> flip runState (Env mempty 0 0)
        >>> \(_, e) -> Right $ toList (instructions e)

braingenStm :: Maybe Text -> B.Stmt -> BgM ()
braingenStm breakpoint stmt = case stmt of
    B.BStmt block -> mapM_ (braingenStm breakpoint) block
    B.Decl t (B.Id i) -> do
        let var = Variable i
            ty = braingenType t
        output $ Alloca var ty
        output $ Store (ConstArgument (Just ty) (defaultValue ty)) var
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
    B.Loop expr stmt -> do
        start <- getLabel "loop_start"
        continue <- getLabel "loop_continue"
        exit <- getLabel "loop_exit"
        output $ Jump continue
        output $ Label continue
        exprVar <- braingenExpr expr
        output $ Br exprVar start exit
        output $ Label start
        mapM_ (braingenStm (Just exit)) stmt
        output $ Jump continue
        output $ Label exit
    B.SExp expr -> void $ braingenExpr expr
    B.Break -> do
        let bp = case breakpoint of
                Just bp -> bp
                Nothing -> error "break outside loop, report as INSERT BUG HERE :)"
        output . Jump $ bp

defaultValue :: Type -> Lit
defaultValue = \case
           I32 -> LitInt 0
           I1 -> LitInt 1
           I8 -> LitInt 0
           F64 -> LitDouble 0
           Ptr -> LitNull
           Void -> LitNull
           FunPtr _ _ -> LitNull
           Array _ _ -> LitNull
           CustomType _ -> LitNull

braingenExpr :: B.Expr -> BgM Variable
braingenExpr (ty, e) = case e of
    B.EGlobalVar (B.Id ident) -> do
        return (ConstVariable ident)
    B.EVar (B.Id ident) -> do
        var <- getTempVariable
        output $ Load var (braingenType ty) (Variable ident)
        return var
    B.ELit lit -> braingenLit lit
    B.Neg e -> do
        var <- braingenExpr e
        tmp <- getTempVariable
        case ty of
            B.Double -> do
                output $ Fneg tmp F64 (Argument Nothing var)
            B.Int -> do
                output $
                    Arith
                        tmp
                        Sub
                        I32
                        (ConstArgument Nothing (LitInt 0))
                        (Argument Nothing var)
            _ -> error "TYPECHECK BUG: Negation of non-number"
        return tmp
    B.Not e -> do
        exprVar <- braingenExpr e
        var <- getTempVariable
        output $ ICmp var Ieq I1 (Argument Nothing exprVar) (ConstArgument Nothing (LitBool False))
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
        var <- getTempVariable
        case ty of
            B.Int -> do
                output $ ICmp var (iRelOp op) I32 (Argument Nothing left) (Argument Nothing right)
                return var
            B.Double -> do
                output $ FCmp var (fRelOp op) F64 (Argument Nothing left) (Argument Nothing right)
                return var
            B.Boolean -> do
                output $ ICmp var (iRelOp op) I1 (Argument Nothing left) (Argument Nothing right)
                return var
            ty -> error $ "TYPECHECK BUG: Relational comparison on invalid type: " <> show ty
    B.EOr l r -> do
        true <- getLabel "true"
        false <- getLabel "false"
        lazyLogical l r True Or false true
    B.EAnd l r -> do
        true <- getLabel "true"
        false <- getLabel "false"
        lazyLogical l r False And false true

lazyLogical ::
    B.Expr ->
    B.Expr ->
    Bool ->
    (Variable -> Type -> Argument -> Argument -> Stmt) ->
    Text ->
    Text ->
    BgM Variable
lazyLogical l r comp operator doneOn continueOn = do
    var <- getTempVariable
    output $ Alloca var I1
    l <- braingenExpr l
    var2 <- getTempVariable
    output $
        ICmp
            var2
            Ieq
            I1
            (Argument Nothing l)
            (ConstArgument Nothing (LitBool comp))
    output $ Store (Argument (Just I1) l) var
    var3 <- getTempVariable
    output $ Load var3 I1 var
    if comp
        then output $ Br var3 continueOn doneOn
        else output $ Br var3 doneOn continueOn
    output $ Label doneOn
    r <- braingenExpr r
    var4 <- getTempVariable
    output $ operator var4 I1 (Argument Nothing var3) (Argument Nothing r)
    output $ Store (Argument (Just I1) var4) var
    output $ Jump continueOn
    output $ Label continueOn
    var5 <- getTempVariable
    output $ Load var5 I1 var
    return var5

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
    pure $ toTitle t <> "." <> thow current

{-| Return a temp variable, useful when calculating intermediate values.

For example: @int x = 1 + 2 + 3@ might generate:

@
%0 = add i32 1 2
@

@
%x = add i32 %0 3
@

And @%_0@ can be obtained by calling @getTempVariable@.
-}
getTempVariable :: BgM Variable
getTempVariable = do
    v <- gets varCounter
    modify (\s -> s {varCounter = v + 1})
    return (Variable $ "_" <> thow v)

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
