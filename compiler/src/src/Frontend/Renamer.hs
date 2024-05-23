{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Renamer (rename) where

import Control.Monad (unless, void, when)
import Control.Monad.Except
import Control.Monad.State
import Data.List.NonEmpty hiding (nubBy, reverse)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Frontend.Error (FEError (..), Report (report), convert)
import Frontend.Parser.ParserTypes
import Utils
import Prelude hiding (head)

-- Make mapping from old names to new names as well
data Env = Env
    { variables :: NonEmpty (Map Id Id)
    , varCounter :: Int
    , functions :: Set Id
    , structs :: Map Id [Id]
    , typeDefs :: Map Id Id
    }

newtype RnM a = Rn {runRm :: StateT Env (Except FEError) a}
    deriving (Functor, Applicative, Monad, MonadError FEError, MonadState Env)

initEnv :: Env
initEnv =
    Env
        (singleton (Map.singleton (IdD NoInfo "length") (IdD NoInfo "length")))
        0
        ( Set.fromList
            [ IdD NoInfo "printInt"
            , IdD NoInfo "printString"
            , IdD NoInfo "printDouble"
            , IdD NoInfo "readInt"
            , IdD NoInfo "readDouble"
            , IdD NoInfo "readString"
            ]
        )
        mempty
        mempty

rename :: Prog -> Either Text Prog
rename p = case runExcept $ flip evalStateT initEnv $ runRm $ rnProg p of
    Left err -> Left $ report err
    Right res -> return res

rnProg :: Prog -> RnM Prog
rnProg (Program i defs) = mapM_ addDef defs >> Program i <$> mapM rnDef defs
  where
    addDef :: TopDef -> RnM ()
    addDef = \case
        tp@(FnDef info _ name _ _) -> do
            funcs <- gets functions
            when (Set.member name funcs) $ throwError $ DuplicateTopDef info tp
            modify $ \s -> s {functions = Set.insert name s.functions}
        tp@(StructDef info name args) -> do
            strcts <- gets structs
            -- NOTE: Overlapping names for typedefs and structs ok
            -- defs <- gets typeDefs
            -- when (Map.member name defs) $ throwError $ DuplicateTopDef info tp
            when (Map.member name strcts) $ throwError $ DuplicateTopDef info tp
            modify $ \s -> s {structs = Map.insert name (fmap (\(Argument _ _ name) -> name) args) s.structs}
        tp@(TypeDef info name1 name2) -> do
            -- NOTE: Overlapping names for typedefs and structs ok
            -- strcts <- gets structs
            -- when (Set.member name2 strcts) $ throwError $ DuplicateTopDef info tp
            defs <- gets typeDefs
            when (Map.member name2 defs) $ throwError $ DuplicateTopDef info tp
            modify $ \s -> s {typeDefs = Map.insert name2 name1 s.typeDefs}
        (Use _ _) -> pure ()

rnDef :: TopDef -> RnM TopDef
rnDef = \case
    FnDef info ty name args stmts -> do
        ty <- rnType ty
        (args, stmts) <- newBlock $ do
            args <- mapM rnArg args
            stmts <- mapM rnStmt stmts
            return (args, stmts)
        return (FnDef info ty name args stmts)
    -- Somewhat ugly to go over typedefs twice, but they can be declared in an
    -- arbitrary order
    topdef@(TypeDef info name1 _) -> do
        gets (Map.member name1 . structs) >>= \case
            False -> throwError $ UnboundStruct info (convert name1)
            True -> return topdef
    -- Same here
    StructDef info name args -> do
        -- Reverse for the redefined field be the one shown in the error
        unique info (reverse args)
        return (StructDef info name args)
    e@Use {} -> pure e

unique :: SynInfo -> [Arg] -> RnM ()
unique _ [] = return ()
unique info (x : xs)
    | any (isArg x) xs = throwError $ DuplicateArgument info x
    | otherwise = unique info xs
  where
    isArg :: Arg -> Arg -> Bool
    isArg (Argument _ _ name1) (Argument _ _ name2) = name1 `is` name2

rnType :: Type -> RnM Type
rnType = return

-- TODO: make differenet scopes for fields and variables
rnField :: SynInfo -> Id -> RnM Id
rnField _ (IdD info' "length") = return (IdD info' "length")
rnField info name = do
    fields <- gets (concat . Map.elems . structs)
    unless (any (is name) fields) (throwError (UnboundField info name))
    return name

is :: Id -> Id -> Bool
is (Id _ na a) (Id _ nb b) = na == nb && a == b

rnId :: SynInfo -> Id -> RnM Id
rnId info id = do
    gets (Set.member id . functions) >>= \case
        True -> return id
        False -> do
            (x :| xs) <- gets variables
            case findVar id (x : xs) of
                Nothing -> throwError $ UnboundVariable info (convert id)
                Just id -> return id
  where
    findVar :: Id -> [Map Id Id] -> Maybe Id
    findVar _ [] = Nothing
    findVar ident (x : xs) = case Map.lookup ident x of
        Nothing -> findVar ident xs
        Just ident' -> return ident'

rnArg :: Arg -> RnM Arg
rnArg arg@(Argument info ty id) =
    gets (Map.member id . head . variables)
        >>= \case
            False ->
                Argument info <$> rnType ty <*> do
                    n <- counter
                    let id' = newId id n
                    insertVar id id'
                    return id'
            True -> throwError $ DuplicateArgument info arg

rnStmt :: Stmt -> RnM Stmt
rnStmt = \case
    Empty info -> return $ Empty info
    BStmt info stmts -> BStmt info <$> newBlock (mapM rnStmt stmts)
    Decl info ty items -> do
        Decl info <$> rnType ty <*> mapM rnItem items
    Ass info lv expr -> Ass info <$> rnExpr lv <*> rnExpr expr
    Incr info id -> Incr info <$> rnId info id
    Decr info id -> Decr info <$> rnId info id
    Ret info expr -> Ret info <$> rnExpr expr
    VRet info -> return (VRet info)
    Cond info expr stmt -> Cond info <$> rnExpr expr <*> newBlock (rnStmt stmt)
    CondElse info expr stmt1 stmt2 ->
        CondElse info
            <$> rnExpr expr
            <*> newBlock (rnStmt stmt1)
            <*> newBlock (rnStmt stmt2)
    While info expr stmt -> While info <$> rnExpr expr <*> newBlock (rnStmt stmt)
    ForEach info arg expr stmt -> do
        expr <- rnExpr expr
        (arg, stmt) <- newBlock $ do
            arg <- rnArg arg
            stmt <- rnStmt stmt
            return (arg, stmt)
        return (ForEach info arg expr stmt)
    Break info -> return (Break info)
    SExp info expr -> SExp info <$> rnExpr expr

rnItem :: Item -> RnM Item
rnItem = \case
    Init info id expr -> do
        gets (Map.lookup id . head . variables) >>= \case
            Nothing -> do
                i <- counter
                expr <- rnExpr expr
                let id' = newId id i
                insertVar id id'
                return $ Init info id' expr
            Just id -> throwError $ BoundVariable info (convert id)
    NoInit info id -> do
        gets (Map.lookup id . head . variables) >>= \case
            Nothing -> do
                i <- counter
                let id' = newId id i
                insertVar id id'
                return (NoInit info id')
            Just id -> throwError $ BoundVariable info (convert id)

insertVar :: Id -> Id -> RnM ()
insertVar old new = do
    (h :| rest) <- gets variables
    modify (\s -> s {variables = Map.insert old new h :| rest})

newId :: Id -> Int -> Id
newId (Id info ns i) n = Id info ns (i <> "$" <> thow n)

rnExpr :: Expr -> RnM Expr
rnExpr = \case
    EVar info id -> EVar info <$> rnId info id
    ELitInt info n -> return (ELitInt info n)
    ELitDouble info n -> return (ELitDouble info n)
    ELitTrue info -> return (ELitTrue info)
    ELitFalse info -> return (ELitFalse info)
    ELitNull info ty -> return (ELitNull info ty)
    EString info text -> return (EString info text)
    EDeref info l r -> EDeref info <$> rnExpr l <*> rnField info r
    EApp info id exprs -> EApp info <$> rnId info id <*> mapM rnExpr exprs
    Neg info expr -> Neg info <$> rnExpr expr
    Not info expr -> Not info <$> rnExpr expr
    EMul info l op r -> EMul info <$> rnExpr l <*> return op <*> rnExpr r
    EAdd info l op r -> EAdd info <$> rnExpr l <*> return op <*> rnExpr r
    ERel info l op r -> ERel info <$> rnExpr l <*> return op <*> rnExpr r
    EAnd info l r -> EAnd info <$> rnExpr l <*> rnExpr r
    EOr info l r -> EOr info <$> rnExpr l <*> rnExpr r
    ENew info ty size -> do
        case ty of
            TVar _ id -> do
                isTypeDef <- gets (Map.member id . typeDefs)
                isStruct <- gets (Map.member id . structs)
                unless (isTypeDef || isStruct) (throwError $ UnboundStruct info (convert id))
            _ -> return ()
        size <- mapM rnExpr size
        return $ ENew info ty size
    EIndex info e1 e2 -> EIndex info <$> rnExpr e1 <*> rnExpr e2
    EStructIndex info e1 field ->
        EStructIndex info
            <$> rnExpr e1
            <*> rnField info field

newBlock :: (MonadState Env m) => m a -> m a
newBlock ma = do
    before <- gets variables
    modify $ \s -> s {variables = mempty <| before}
    a <- ma
    modify $ \s -> s {variables = before}
    return a

counter :: RnM Int
counter = do
    i <- gets varCounter
    modify $ \s -> s {varCounter = s.varCounter + 1}
    return i
