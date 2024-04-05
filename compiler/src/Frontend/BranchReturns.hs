{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Frontend.BranchReturns where

import BrainletteParser
import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Control.Monad.Extra (anyM, unless)
import Control.Monad.Reader (MonadReader, local, ReaderT (runReaderT), ask)
import Control.Monad.State (MonadState, StateT, execStateT, get, put)
import Data.Fixed (mod')
import Data.Functor (($>))
import Data.List (intersperse)
import Data.Maybe (listToMaybe)
import Data.String.Interpolate
import Data.Text (Text, concat, pack, takeWhile, unlines)
import ParserTypes
import Prelude hiding (concat, takeWhile, unlines)

data Env = Env {unreachables :: [Stmt], missingReturn :: [TopDef]}
    deriving (Show)

newtype Br a = Br {runBr :: StateT Env (ReaderT Bool (Except Text)) a}
    deriving (Functor, Applicative, Monad, MonadState Env, MonadReader Bool, MonadError Text)

check :: Prog -> Either Text Prog
check p = case runExcept $ flip runReaderT False $ flip execStateT (Env mempty mempty) $ runBr $ branchReturns p of
    Left err -> Left err
    Right (Env [] []) -> Right p
    Right (Env stmts topdefs) ->
        Left $
            concat (intersperse "\n\n" $ map errUnreachable stmts)
                <> unlines (map errMissingRet topdefs)

errUnreachable :: Stmt -> Text
errUnreachable stmt = "unreachable statement\n" <> reportStmt stmt

reportStmt :: Stmt -> Text
reportStmt stmt =
    let info = hasInfo stmt
     in quote (takeWhile (/= '\n') info.sourceCode)
            <> " at "
            <> pack (show info.sourceLine)
            <> ":"
            <> pack (show info.sourceColumn)

errMissingRet :: TopDef -> Text
errMissingRet (FnDef info _ _ _ stmts) = case stmts of
    [] ->
        "missing return in\n"
            <> info.sourceCode
            <> "\n"
            <> "at "
            <> pack (show info.sourceLine)
            <> ":"
            <> pack (show info.sourceColumn)
    xs ->
        "missing return in function "
            <> quote info.sourceName
            <> "\ngot "
            <> "\n  "
            <> reportStmt (last xs)
            <> "\nexpected\n  a return statement"

quote :: Text -> Text
quote s = "'" <> s <> "'"

unreachable :: Stmt -> Br ()
unreachable s = do
    env <- get
    put (env {unreachables = s : env.unreachables})

missingFn :: TopDef -> Br ()
missingFn fn = do
    env <- get
    put (env {missingReturn = fn : env.missingReturn})

branchReturns :: Prog -> Br ()
branchReturns (Program _ topdefs) = mapM_ breakDefs topdefs >> mapM_ retDefs topdefs

breakDefs :: TopDef -> Br ()
breakDefs (FnDef _ _ _ _ stmts) = mapM_ breaks stmts

-- TODO: Fix much prettier error msg
breaks :: Stmt -> Br ()
breaks = \case
    Empty _ -> return ()
    BStmt _ stmts -> mapM_ breaks stmts
    Decl {} -> return ()
    Ass {} -> return ()
    Incr {} -> return ()
    Decr {} -> return ()
    Ret _ _ -> return ()
    VRet _ -> return ()
    Cond _ _ stmt -> breaks stmt
    CondElse _ _ stmt1 stmt2 -> breaks stmt1 >> breaks stmt2
    While _ _ stmt -> local (const True) $ breaks stmt
    SExp _ _ -> return ()
    Break info -> do
        b <- ask
        unless b $
            throwError
                [i|break outside loop\n#{sourceCode info}\n#{sourceLine info}:#{sourceColumn info}|]

retDefs :: TopDef -> Br ()
retDefs self@(FnDef _ _ _ _ stmts) =
    returns stmts >>= \case
        True -> return ()
        False -> missingFn self
  where
    returns :: [Stmt] -> Br Bool
    returns [] = return False
    returns (x : xs) = do
        b <- returnsStmt x
        if b
            then maybe (return ()) unreachable (listToMaybe xs) >> return True
            else returns xs

returnsStmt :: Stmt -> Br Bool
returnsStmt = \case
    Empty _ -> return False
    BStmt _ stmts -> anyM returnsStmt stmts
    Decl {} -> return False
    Ass {} -> return False
    Incr _ _ -> return False
    Decr _ _ -> return False
    Ret _ _ -> return True
    VRet _ -> return True
    Cond _ expr stmt ->
        if never expr
            then unreachable stmt $> False
            else (always expr &&) <$> returnsStmt stmt
    CondElse _ expr stmt1 stmt2 -> do
        if
            | never expr -> unreachable stmt1 >> returnsStmt stmt2
            | always expr -> unreachable stmt2 >> returnsStmt stmt1
            | otherwise -> (&&) <$> returnsStmt stmt1 <*> returnsStmt stmt2
    While _ expr stmt ->
        if never expr
            then unreachable stmt $> False
            else (always expr &&) <$> returnsStmt stmt
    SExp _ _ -> return False
    Break _ -> return False

always :: Expr -> Bool
always e = case interpret e of
    Just (IsBool True) -> True
    _ -> False

never :: Expr -> Bool
never e = case interpret e of
    Just (IsBool False) -> False
    _ -> False

interpret :: Expr -> Maybe Value
interpret = \case
    EVar {} -> Nothing
    ELitInt _ n -> return $ IsInt n
    ELitDouble _ n -> return $ IsDouble n
    ELitTrue _ -> return $ IsBool True
    ELitFalse _ -> return $ IsBool False
    EApp {} -> Nothing
    EString _ str -> return $ IsString str
    Neg _ e -> case interpret e of
        Just (IsDouble n) -> return $ IsDouble (-n)
        Just (IsInt n) -> return $ IsInt (-n)
        _ -> Nothing
    Not _ e -> case interpret e of
        Just (IsBool b) -> Just (IsBool (not b))
        _ -> Nothing
    EMul _ l op r -> do
        l' <- interpret l
        r' <- interpret r
        case (l', r') of
            (IsInt n, IsInt m) -> case op of
                Times {} -> Just $ IsInt $ n * m
                Div {} -> Just $ IsInt $ n `div` m
                Mod {} -> Just $ IsInt $ n `mod` m
            (IsDouble n, IsInt m) -> case op of
                Times {} -> Just $ IsDouble $ n * fromInteger m
                Div {} -> Just $ IsDouble $ n / fromInteger m
                Mod {} -> Just $ IsDouble $ n `mod'` fromInteger m
            (IsInt n, IsDouble m) -> case op of
                Times {} -> Just $ IsDouble $ fromInteger n * m
                Div {} -> Just $ IsDouble $ fromInteger n / m
                Mod {} -> Just $ IsDouble $ fromInteger n `mod'` m
            (IsDouble n, IsDouble m) -> case op of
                Times {} -> Just $ IsDouble $ n * m
                Div {} -> Just $ IsDouble $ n / m
                Mod {} -> Just $ IsDouble $ n `mod'` m
            _ -> Nothing
    EAdd _ l op r -> do
        l' <- interpret l
        r' <- interpret r
        case (l', r') of
            (IsInt n, IsInt m) -> Just $ IsInt $ addOp op n m
            (IsDouble n, IsInt m) ->
                Just $ IsDouble $ addOp op n (fromInteger m)
            (IsInt n, IsDouble m) ->
                Just $ IsDouble $ addOp op (fromInteger n) m
            (IsDouble n, IsDouble m) -> Just $ IsDouble $ addOp op n m
            _ -> Nothing
    EOr _ l r -> do
        l' <- interpret l
        r' <- interpret r
        case (l', r') of
            (IsBool b1, IsBool b2) -> Just $ IsBool (b1 || b2)
            _ -> Nothing
    EAnd _ l r -> do
        l' <- interpret l
        r' <- interpret r
        case (l', r') of
            (IsBool b1, IsBool b2) -> Just $ IsBool (b1 && b2)
            _ -> Nothing
    ERel _ l op r -> do
        l' <- interpret l
        r' <- interpret r
        case (l', r') of
            (IsString s1, IsString s2) -> Just $ IsBool $ relOp op s1 s2
            (IsDouble n, IsDouble m) -> Just $ IsBool $ relOp op n m
            (IsInt n, IsInt m) -> Just $ IsBool $ relOp op n m
            (IsInt n, IsDouble m) ->
                Just $ IsBool $ relOp op (fromInteger n) m
            (IsDouble n, IsInt m) ->
                Just $ IsBool $ relOp op n (fromInteger m)
            _ -> Nothing

relOp :: RelOp -> ((Ord a) => a -> a -> Bool)
relOp (LTH _) = (<)
relOp (LE _) = (<=)
relOp (GTH _) = (>)
relOp (GE _) = (>=)
relOp (EQU _) = (==)
relOp (NE _) = (/=)

addOp :: AddOp -> ((Num a) => a -> a -> a)
addOp (Plus _) = (+)
addOp (Minus _) = (-)

data Value = IsBool Bool | IsInt Integer | IsString Text | IsDouble Double
    deriving (Show, Eq, Ord)
