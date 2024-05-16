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
import Utils (Pretty (..), concatFor, thow)
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
    B.BStmt block -> do
        comment "block"
        mapM_ (braingenStm breakpoint) block
        comment "block end"
    B.Decl t (B.Id i) -> do
        comment $ "decl: " <> i
        alloca (Variable i) (braingenType t)
        comment "decl done"
    B.Ass _ (B.LVar (B.Id a)) expr@(t, _) -> do
        comment $ "store: " <> thow t
        argument <- case expr of
            (_, B.ELit l) -> return (ConstArgumentAuto (lit l))
            _ -> do
                result <- braingenExpr expr
                return (Argument (pure $ braingenType t) result)
        store argument (Variable a)
        comment "store done"
    B.Ass ty (B.LIndex arr index) expr -> do
        comment "index ass"
        let ty' = braingenType ty
        arr <- braingenExpr arr
        index <- braingenExpr index
        ptr <- getTempVariable
        comment "One"
        getElementPtr
            ptr
            ty'
            (Argument (Just Ptr) arr)
            (Argument (Just I64) index)
        var <- braingenExpr expr
        store (Argument (Just ty') var) ptr
        comment "index ass done"
    B.Ass ty (B.LDeref e@(innerE, _) i) expr -> do
        comment "deref ass"
        let ty' = braingenType ty
        let tyE = braingenType innerE
        e <- braingenExpr e
        ptr <- getTempVariable
        comment "Two"
        comment $ thow ty
        getElementPtr
            ptr
            (braingenType innerE)
            (Argument (Just tyE) e)
            (ConstArgument (Just I32) (LitInt (fromIntegral i)))
        var <- braingenExpr expr
        store (Argument (Just ty') var) ptr
        comment "deref ass done"
    B.Ass ty1 (B.LStructIndex e@(_, _) i) expr -> do
        comment "structindex ass"
        let ty1' = braingenType ty1
        e <- case e of
            (_, B.EVar (B.Id v)) -> return (Variable v)
            _ -> braingenExpr e
        -- e <- braingenExpr e
        ptr <- getTempVariable
        comment "Three"
        getElementPtr
            ptr
            ty1'
            (Argument (Just Ptr) e)
            (ConstArgument (Just I64) (LitInt $ fromIntegral i))
        comment $ "HERE IT IS: " <> thow expr
        comment $ "EXPRESSION: " <> thow expr
        var <- braingenExpr expr
        store (Argument (Just ty1') var) ptr
        comment "structindex ass done"
    B.Ret (Just (ty, B.ELit l)) -> do
        ty <- return (braingenType ty)
        ret (ConstArgument (Just ty) (lit l))
    B.Ret (Just expr@(t, _)) -> do
        comment $ "ret: " <> thow t
        result <- braingenExpr expr
        ret (Argument (pure $ braingenType t) result)
        comment "ret done"
    B.Ret Nothing -> do
        comment "ret void"
        retVoid
        comment "ret void done"
    B.CondElse expr s1 s2 -> do
        comment "condelse"
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
        comment "condelse done"
    B.Loop expr stmt -> do
        comment "loop"
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
        comment "loop done"
    B.SExp expr -> do
        comment $ "sexp: " <> thow expr
        void $ braingenExpr expr
        comment "sexp done"
    B.ArrayAlloc ty (B.Id name) (_, sz) -> do
        let var = Variable name
        alloca var (braingenType ty)
        arrSize <- braingenExpr sz
        addr <- getTempVariable
        malloc addr arrSize
        store (Argument (Just Ptr) addr) var
    B.Break -> do
        comment "break"
        let bp = case breakpoint of
                Just bp -> bp
                Nothing ->
                    error "break outside loop, report as INSERT BUG HERE :)"
        jump bp
        comment "break done"

braingenExpr :: B.Expr -> BgM Variable
braingenExpr ogExpression@(ty, e) = case e of
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
    B.StructInit False vals -> do
        var <- getTempVariable
        comment "stack allocating a struct"
        alloca var (braingenType ty)
        forM_ (zip [0 ..] vals) \(i, (t, v)) -> do
            ptr <- getTempVariable
            comment "Four"
            getElementPtr
                ptr
                (braingenType t)
                (Argument (Just Ptr) var)
                (ConstArgument (Just I64) (LitInt i))
            comment "START"
            comment (thow $ braingenType t)
            comment $ thow (lit v)
            comment "STOP"
            store (ConstArgument (Just $ braingenType t) (lit v)) ptr
        temp <- getTempVariable
        load temp (braingenType ty) var
        comment "stack allocating a struct done"
        return temp
    B.StructInit True vals -> do
        sizeVar <- braingenExpr (mkLitIntE (sum (map (sizeOf . braingenType . fst) vals)))
        var1 <- getTempVariable
        malloc var1 sizeVar

        forM_ (zip [0 ..] vals) \(i, (t, v)) -> do
            ptr <- getTempVariable
            comment "Five"
            comment $ thow e
            getElementPtr
                ptr
                (braingenType t)
                (Argument (Just . braingenType $ ty) var1)
                (ConstArgument (Just I32) (LitInt i))
            store (ConstArgument (Just $ braingenType t) (lit v)) ptr
        return var1
    B.Deref e i -> do
        let ty' = braingenType ty
        e' <- braingenExpr e
        ptr <- getTempVariable
        comment "Six"
        getElementPtr
            ptr
            ty'
            (Argument (Just (braingenType $ typeOf e)) e')
            (ConstArgument (Just I64) (LitInt (fromIntegral i)))
        var <- getTempVariable
        load var ty' ptr
        return var
    B.ArrayInit _ -> error "TODO: {EAllocInit} Does not exist yet"
    B.ArrayIndex base index -> do
        baseVar <- braingenExpr base
        indexVar <- braingenExpr index

        -- TODO: add bounds checking

        resPtr <- getTempVariable
        comment "Seven"
        comment (thow $ braingenType ty)
        getElementPtr
            resPtr
            (braingenType ty)
            (Argument (Just Ptr) baseVar)
            (Argument (Just I64) indexVar)

        res <- getTempVariable
        load res (braingenType ty) resPtr

        pure res
    B.StructIndex e@(ty, _) i -> do
        comment $ thow ogExpression
        e <- braingenExpr e
        var <- getTempVariable
        comment "Eight"
        -- getElementPtr var (braingenType ty) (Argument (Just Ptr) e) (ConstArgument (Just I64) (LitInt (fromIntegral i)))
        extractValue var (braingenType ty) e (fromIntegral i)
        return var

mkLitIntE :: Integer -> B.Expr
mkLitIntE n = (B.Int, B.ELit $ B.LitInt n)

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
        alloca intermediate ty
        store (ConstArgument (pure ty) LitNull) intermediate
        var <- getTempVariable
        load var ty intermediate
        return var
    B.LitArrNull -> error "TODO: `braingenLit` LitArrNul"

lit :: B.Lit -> Lit
lit = \case
    B.LitInt i -> LitInt i
    B.LitDouble d -> LitDouble d
    B.LitBool b -> LitBool b
    B.LitString _ -> error "CODEGEN BUG: String literal still exist"
    B.LitNull -> LitNull
    B.LitArrNull -> LitArrNull

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
    B.Pointer _ -> Ptr
    B.Array _ -> CustomType "Array$Internal"
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
    ty -> error $ "error: report bug as a typeerror" <> show ty

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

arrayType :: Type
arrayType = CustomType "Array$Internal"

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
    CustomType "Array$Internal" -> 16
    CustomType _ -> 8

typeOf :: B.Expr -> B.Type
typeOf (ty, _) = ty
