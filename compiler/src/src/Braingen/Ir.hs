{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Braingen.Ir where

import BMM.Bmm qualified as B
import Braingen.LlvmAst
import Braingen.Output (OutputIr (outputIr))
import Braingen.TH
import Control.Arrow ((>>>))
import Control.Monad (void)
import Control.Monad.State (State, get, gets, modify, put, runState)
import Data.DList hiding (map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack, toTitle)
import Utils (concatFor, thow)
import Prelude hiding (takeWhile)

$(gen "Stmt")

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
braingen :: B.Prog -> Text
braingen = outputIr . braingenProg

braingenProg :: B.Prog -> Ir
braingenProg (B.Program tp) = do
    Ir (map braingenTopDef tp)

braingenTopDef :: B.TopDef -> TopDef
braingenTopDef def = case def of
    B.StringGlobal name string -> ConstantString name string
    B.FnDef rt (B.Id i) a s -> do
        let ret = braingenType rt
        let args = map (appendArgName "arg" . braingenArg) a
        let argStmts = concatFor a argToStmts
        let stmts = braingenStmts s
        let stmts' =
                stmts
                    ++ case rt of
                        B.Void -> [RetVoid, Unreachable]
                        _ -> [Unreachable]

        Define ret i args Nothing (argStmts <> stmts')
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

braingenStmts :: [B.Stmt] -> [Stmt]
braingenStmts =
    mapM_ (braingenStm Nothing)
        >>> flip runState (Env mempty 0 0)
        >>> \(_, e) -> toList (instructions e)

braingenStm :: Maybe Text -> B.Stmt -> BgM ()
braingenStm breakpoint stmt = case stmt of
    B.BStmt block -> mapM_ (braingenStm breakpoint) block
    B.Decl t (B.Id i) -> do
        let var = Variable i
            ty = braingenType t
        alloca var ty
        store (ConstArgument (Just ty) (defaultValue ty)) var
    B.Ass (B.Id a) expr@(t, _) -> do
        result <- braingenExpr expr
        store (Argument (pure $ braingenType t) result) (Variable a)
    B.Ret (Just expr@(t, _)) -> do
        result <- braingenExpr expr
        ret (Argument (pure $ braingenType t) result)
    B.Ret Nothing -> do
        retVoid
    B.CondElse expr s1 s2 -> do
        result <- braingenExpr expr
        lTrue <- getLabel "IfTrue"
        lFalse <- getLabel "IfFalse"
        lDone <- getLabel "IfDone"
        -- if
        br result lTrue lFalse
        -- if true
        label lTrue
        mapM_ (braingenStm (Just lDone)) s1
        jump lDone
        -- if false
        label lFalse
        mapM_ (braingenStm (Just lDone)) s2
        jump lDone
        -- if done
        label lDone
    B.Loop expr stmt -> do
        start <- getLabel "loop_start"
        continue <- getLabel "loop_continue"
        exit <- getLabel "loop_exit"
        jump continue
        label continue
        exprVar <- braingenExpr expr
        br exprVar start exit
        label start
        mapM_ (braingenStm (Just exit)) stmt
        jump continue
        label exit
    B.SExp expr -> void $ braingenExpr expr
    B.Break -> do
        let bp = case breakpoint of
                Just bp -> bp
                Nothing ->
                    error "break outside loop, report as INSERT BUG HERE :)"
        jump bp

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
        load var (braingenType ty) (Variable ident)
        return var
    B.ELit lit -> braingenLit lit
    B.Neg e -> do
        var <- braingenExpr e
        tmp <- getTempVariable
        case ty of
            B.Double -> do
                fneg tmp F64 (Argument Nothing var)
            B.Int -> do
                arith
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
        iCmp
            var
            Ieq
            I1
            (Argument Nothing exprVar)
            (ConstArgument Nothing (LitBool False))
        return var
    B.EApp (B.Id func) args -> do
        args <-
            mapM
                ( \e@(t, _) ->
                    Argument (Just $ braingenType t) <$> braingenExpr e
                )
                args
        case ty of
            B.Void -> do
                voidCall Nothing Nothing (braingenType ty) func args
                return (error "CODEGEN: tried referencing void variable")
            _ -> do
                result <- getTempVariable
                call result Nothing Nothing (braingenType ty) func args
                return result
    B.EAdd e1 op e2 -> do
        let t = braingenType ty
        r1 <- braingenExpr e1
        r2 <- braingenExpr e2
        let op' = braingenAddOp t op
        res <- getTempVariable
        arith res op' t (Argument Nothing r1) (Argument Nothing r2)
        pure res
    B.EMul e1 op e2 -> do
        let t = braingenType ty
        r1 <- braingenExpr e1
        r2 <- braingenExpr e2
        let op' = braingenMulOp t op
        res <- getTempVariable
        arith res op' t (Argument Nothing r1) (Argument Nothing r2)
        pure res
    B.Cast e@(t, _) -> do
        let ty' = braingenType ty
        let t' = braingenType t
        val <- braingenExpr e
        res <- getTempVariable
        cast (getCastOp ty' t') res t' val ty'
        pure res
    B.ERel (ty, l) op r -> do
        left <- braingenExpr (ty, l)
        right <- braingenExpr r
        var <- getTempVariable
        case ty of
            B.Int -> do
                iCmp
                    var
                    (iRelOp op)
                    I32
                    (Argument Nothing left)
                    (Argument Nothing right)
                return var
            B.Double -> do
                fCmp
                    var
                    (fRelOp op)
                    F64
                    (Argument Nothing left)
                    (Argument Nothing right)
                return var
            B.Boolean -> do
                iCmp
                    var
                    (iRelOp op)
                    I1
                    (Argument Nothing left)
                    (Argument Nothing right)
                return var
            ty ->
                error $
                    "TYPECHECK BUG: Relational comparison on invalid type: "
                        <> show ty
    B.EOr l r -> do
        true <- getLabel "true"
        false <- getLabel "false"
        lazyLogical l r True Braingen.Ir.or false true
    B.EAnd l r -> do
        true <- getLabel "true"
        false <- getLabel "false"
        lazyLogical l r False Braingen.Ir.and false true

lazyLogical ::
    B.Expr ->
    B.Expr ->
    Bool ->
    (Variable -> Type -> Argument -> Argument -> BgM ()) ->
    Text ->
    Text ->
    BgM Variable
lazyLogical l r comp operator doneOn continueOn = do
    var <- getTempVariable
    alloca var I1
    l <- braingenExpr l
    var2 <- getTempVariable
    iCmp
        var2
        Ieq
        I1
        (Argument Nothing l)
        (ConstArgument Nothing (LitBool comp))
    store (Argument (Just I1) l) var
    var3 <- getTempVariable
    load var3 I1 var
    if comp
        then br var3 continueOn doneOn
        else br var3 doneOn continueOn
    label doneOn
    r <- braingenExpr r
    var4 <- getTempVariable
    operator var4 I1 (Argument Nothing var3) (Argument Nothing r)
    store (Argument (Just I1) var4) var
    jump continueOn
    label continueOn
    var5 <- getTempVariable
    load var5 I1 var
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
        alloca intermediate ty
        store (ConstArgument (pure ty) (LitInt n)) intermediate
        var <- getTempVariable
        load var ty intermediate
        return var
    B.LitDouble n -> do
        let ty = F64
        intermediate <- getTempVariable
        alloca intermediate ty
        store (ConstArgument (pure ty) (LitDouble n)) intermediate
        var <- getTempVariable
        load var ty intermediate
        return var
    B.LitBool b -> do
        let ty = I1
        intermediate <- getTempVariable
        alloca intermediate ty
        store (ConstArgument (pure ty) (LitBool b)) intermediate
        var <- getTempVariable
        load var ty intermediate
        return var
    B.LitString _ -> error "CODEGEN BUG: String literal still exist"

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
braingenArg (B.Argument t (B.Id i)) =
    Argument (pure $ braingenType t) (Variable i)

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
