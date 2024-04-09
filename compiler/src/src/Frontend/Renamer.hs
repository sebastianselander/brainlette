{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Renamer (rename) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import Data.List.NonEmpty
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
data Env = Env {variables :: NonEmpty (Map Id Id), varCounter :: Int, functions :: Set Id}

newtype RnM a = Rn {runRm :: StateT Env (Except FEError) a}
    deriving (Functor, Applicative, Monad, MonadError FEError, MonadState Env)

rename :: Prog -> Either Text Prog
rename p = case runExcept $ flip evalStateT (Env (singleton mempty) 0 mempty) $ runRm $ rnProg p of
    Left err -> Left $ report err
    Right res -> return res

rnProg :: Prog -> RnM Prog
rnProg (Program i defs) = mapM_ addDef defs >> Program i <$> mapM rnDef defs
  where
    addDef :: TopDef -> RnM ()
    addDef tp@(FnDef info _ name _ _) = do
        funcs <- gets functions
        when (Set.member name funcs) $ throwError $ DuplicateTopDef info tp
        modify $ \s -> s {functions = Set.insert name s.functions}

rnDef :: TopDef -> RnM TopDef
rnDef (FnDef info ty name args stmts) = do
    ty <- rnType ty
    (args, stmts) <- newBlock $ do
        args <- mapM rnArg args
        stmts <- mapM rnStmt stmts
        return (args, stmts)
    return (FnDef info ty name args stmts)

rnType :: Type -> RnM Type
rnType = return

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
rnArg (Argument info ty id) =
    Argument info <$> rnType ty <*> do
        n <- counter
        let id' = newId id n
        insertVar id id'
        return id'

rnStmt :: Stmt -> RnM Stmt
rnStmt = \case
    Empty info -> return $ Empty info
    BStmt info stmts -> BStmt info <$> newBlock (mapM rnStmt stmts)
    Decl info ty items -> do
        Decl info <$> rnType ty <*> mapM rnItem items
    Ass info id expr -> Ass info <$> rnId info id <*> rnExpr expr
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
    Break info -> return (Break info)
    SExp info expr -> SExp info <$> rnExpr expr

rnItem :: Item -> RnM Item
rnItem = \case
    Init info id expr -> do
        gets (Map.lookup id . head . variables) >>= \case
            Nothing -> do
                i <- counter
                let id' = newId id i
                insertVar id id'
                Init info id' <$> rnExpr expr
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
newId (Id info i) n = Id info (i <> "$" <> thow n)

rnExpr :: Expr -> RnM Expr
rnExpr = \case
    EVar info id -> EVar info <$> rnId info id
    ELitInt info n -> return (ELitInt info n)
    ELitDouble info n -> return (ELitDouble info n)
    ELitTrue info -> return (ELitTrue info)
    ELitFalse info -> return (ELitFalse info)
    EApp info id exprs -> EApp info <$> rnId info id <*> mapM rnExpr exprs
    EString info text -> return (EString info text)
    Neg info expr -> Neg info <$> rnExpr expr
    Not info expr -> Not info <$> rnExpr expr
    EMul info l op r -> EMul info <$> rnExpr l <*> return op <*> rnExpr r
    EAdd info l op r -> EAdd info <$> rnExpr l <*> return op <*> rnExpr r
    ERel info l op r -> ERel info <$> rnExpr l <*> return op <*> rnExpr r
    EAnd info l r -> EAnd info <$> rnExpr l <*> rnExpr r
    EOr info l r -> EOr info <$> rnExpr l <*> rnExpr r

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
