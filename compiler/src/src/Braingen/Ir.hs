{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Braingen.Ir where

import BMM.Bmm qualified as B
import Braingen.LlvmAst
import Braingen.Output (out)
import Braingen.TH
import Control.Arrow ((>>>))
import Control.Monad (forM_, void)
import Control.Monad.State (State, get, gets, modify, put, runState)
import Data.DList hiding (foldr, map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, takeWhile, toTitle)
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
braingen = out . braingenProg

braingenProg :: B.Prog -> Ir
braingenProg (B.Program tp) = do
    Ir (map braingenTopDef tp)

braingenTopDef :: B.TopDef -> TopDef
braingenTopDef def = case def of
    B.StringGlobal name string -> ConstantString name string
    B.StructDef (B.Id n) fields -> Type n . map braingenType $ fields
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
        output $ Alloca (Variable i) (braingenType t)
    B.Ass _ (B.LVar (B.Id a)) expr@(t, _) -> do
        result <- braingenExpr expr
        output $ Store (Argument (pure $ braingenType t) result) (Variable a)
    B.Ass ty (B.LDeref e@(innerE, _) i) expr -> do
        let ty' = braingenType ty
        let tyE = braingenType innerE
        e <- braingenExpr e
        ptr <- getTempVariable
        output $
            GetElementPtr
                ptr
                ty'
                (Argument (Just tyE) e)
                (ConstArgument (Just I64) (LitInt (fromIntegral i)))
        var <- braingenExpr expr
        output $ Store (Argument (Just ty') var) ptr
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
                    I64
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
                    I64
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
            B.Pointer _ -> do
                -- if op == B.EQU
                --     then call var Nothing Nothing I1 "ptrEq" [Argument (Just Ptr) left, Argument (Just Ptr) right]
                --     else iCmp var (iRelOp op) I64 (Argument Nothing left) (Argument Nothing right)
                iCmp var (iRelOp op) Ptr (Argument Nothing left) (Argument Nothing right)
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
    B.ENew vals -> do
        let size = fromIntegral $ length vals * 8
        var1 <- getTempVariable
        malloc var1 size

        forM_ (zip [0 ..] vals) \(i, (t, v)) -> do
            ptr <- getTempVariable
            getElementPtr
                ptr
                (braingenType t)
                (Argument (Just . braingenType $ ty) var1)
                (ConstArgument (Just I64) (LitInt i))
            store (ConstArgument (Just $ braingenType t) (lit v)) ptr

        return var1
    B.Deref e i -> do
        let ty' = braingenType ty
        e <- braingenExpr e
        ptr <- getTempVariable
        output $
            GetElementPtr
                ptr
                ty'
                (Argument (Just Ptr) e)
                (ConstArgument (Just I64) (LitInt (fromIntegral i)))
        var <- getTempVariable
        output $ Load var ty' ptr
        return var
    B.EAlloc arrSize -> do
        let t' = braingenType ty
        let size = sizeOf t' * arrSize

        addr <- getTempVariable
        malloc addr size

        array <- getTempVariable
        alloca array (CustomType "Array")

        arrayPtr <- getTempVariable
        getElementPtr
            arrayPtr
            Ptr
            (Argument (Just . RawPtr . CustomType $ "Array") addr)
            (ConstArgument (Just I64) (LitInt 0))
        store (Argument (Just Ptr) addr) arrayPtr

        sizeAddr <- getTempVariable
        getElementPtr
            sizeAddr
            Ptr
            (Argument (Just . RawPtr . CustomType $ "Array") addr)
            (ConstArgument (Just I64) (LitInt 1))
        store (ConstArgument (Just I64) (LitInt arrSize)) sizeAddr

        pure array
    B.EIndex _ _ -> do
        comment "EXPR-TODO: EIndex"
        pure (Variable "TODO")

-- _ -> do
--     output . Comment $ "EXPR-TODO: " <> thow e
--     pure (Variable "TODO")

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
braingenLit = \case
    B.LitInt n -> do
        let ty = I64
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
    B.LitNull -> do
        let ty = Ptr
        intermediate <- getTempVariable
        output $ Alloca intermediate ty
        output $ Store (ConstArgument (pure ty) LitNull) intermediate
        var <- getTempVariable
        output $ Load var ty intermediate
        return var

lit :: B.Lit -> Lit
lit = \case
    B.LitInt i -> LitInt i
    B.LitDouble d -> LitDouble d
    B.LitBool b -> LitBool b
    B.LitString _ -> error "CODEGEN BUG: String literal still exist"
    B.LitNull -> LitNull

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
%0 = add I64 1 2
@

@
%x = add I64 %0 3
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
    B.Int -> I64
    B.Boolean -> I1
    B.Double -> F64
    B.Void -> Void
    B.String -> Ptr
    B.TVar (B.Id x) -> CustomType x
    B.Pointer t -> RawPtr (braingenType t)
    B.Array _ -> CustomType "Array"
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
    I64 -> \case
        B.Plus -> Add
        B.Minus -> Sub
    F64 -> \case
        B.Plus -> FAdd
        B.Minus -> FSub
    _ -> error "error: report bug as a typeerror"

-- | Convert a BMM add op to an IR Arithmetic instructions
braingenMulOp :: Type -> B.MulOp -> Arithmetic
braingenMulOp = \case
    I64 -> \case
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
    (F64, I64) -> FPtoSI
    (I64, F64) -> SItoFP
    _ -> Bitcast

consName :: (Show a) => a -> Text
consName = takeWhile (/= ' ') . thow

sizeOf :: Type -> Integer
sizeOf = \case
    I32 -> 4
    I64 -> 8
    I1 -> 1
    I8 -> 1
    F64 -> 8
    Ptr -> 8
    RawPtr _ -> sizeOf Ptr
    Void -> 0
    FunPtr _ _ -> 8
    Array _ _ -> sizeOf Ptr
    CustomType _ -> 8
