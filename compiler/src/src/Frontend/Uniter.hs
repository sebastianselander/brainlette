{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Uniter (importFiles) where

import Control.Monad (forM, unless, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (StateT, get, put, runStateT)
import Data.Generics (listify)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import Frontend.Error (FEError (MissingImport))
import Frontend.Parser.BrainletteParser (program)
import Frontend.Parser.ParserTypes qualified as P
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath
import System.IO (stderr)
import Utils (thow, treeMap)

type Uniter = StateT (Set String) (ExceptT FEError IO)

importFiles :: P.Prog -> IO (Either FEError P.Prog)
importFiles (P.Program a pr) = do
    let fileName = T.unpack . P.sourceName $ a
    let modName = T.pack . takeBaseName $ fileName

    -- Insert our namespace into all Ids that do not already have a namespace
    let pr' = P.Program a (map (fixTopDef modName) pr)

    res <- runExceptT (runStateT (getImports modName pr') (Set.fromList [fileName]))
    pure $ fst <$> res

getImports :: Text -> P.Prog -> Uniter P.Prog
getImports ownMod (P.Program a tds) = do
    let sourceDir = takeDirectory . T.unpack . P.sourceName $ a
    let files = flip mapMaybe tds $
            \case
                P.Use _ (P.Id _ _ id) -> Just (id, sourceDir <> "/" <> T.unpack id <> ".bl")
                _ -> Nothing

    s <- get
    -- Verify that all namespaces in ids are actually imported, otherwise throw an error
    checkIds (allIds tds) (Set.fromList $ ownMod : map fst files)

    -- Get all not already imported modules
    let files' = filter (flip Set.notMember s . snd) files

    -- Append the modules we are importing now to the list of imported modules
    put $ foldr (Set.insert . snd) s files'

    -- Load the new modules
    res <- forM files' $ \(modName, file) -> liftIO $ do
        b <- doesFileExist file
        unless b (hPutStrLn stderr ("brainlette: module '" <> modName <> "' does not exist") >> exitFailure)
        text <- readFile file
        return (modName, file, text)

    -- Parse the module, and shove its namespace into all its ids
    res <- forM res $ \(modName, file, text) -> case program file (T.pack text) of
        Left err -> liftIO $ hPutStrLn stderr (thow err) *> exitFailure
        Right res -> return (modName, fixTree modName res)
    -- Repeat this proces for the new module (i.e import more modules)
    res <- concatMap (\(P.Program _ d) -> d) <$> mapM (uncurry getImports) res
    -- Return the old tree but concatenate in the modules we imported
    pure (P.Program a (filter isNotUse tds <> res))

checkIds :: [P.Id] -> Set Text -> Uniter ()
checkIds (i@(P.Id a ns _) : id) imports = do
    when (isNotImported imports i) (throwError $ MissingImport a (fromJust ns))
    checkIds id imports
checkIds [] _ = pure ()

allIds :: [P.TopDef] -> [P.Id]
allIds = listify (\P.Id {} -> True)

isNotImported :: Set Text -> P.Id -> Bool
isNotImported s (P.Id _ (Just mod) _) = Set.notMember mod s
isNotImported _ _ = False

isNotUse :: P.TopDef -> Bool
isNotUse (P.Use {}) = False
isNotUse _ = True

fixTree :: Text -> P.Prog -> P.Prog
fixTree mod = treeMap (fixTopDef mod)

ignoreList :: Set Text
ignoreList =
    Set.fromList
        [ "printInt"
        , "printString"
        , "printDouble"
        , "readInt"
        , "readDouble"
        , "readString"
        ]

fixId :: Text -> P.Id -> P.Id
fixId mod o@(P.Id i Nothing n) =
    if Set.notMember n ignoreList
        then P.Id i (Just mod) n
        else o
fixId _ i = i

fixTopDef :: Text -> P.TopDef -> P.TopDef
fixTopDef mod = \case
    P.FnDef i rt n@(P.Id _ Nothing "main") args stmts -> P.FnDef i rt n (treeMap (fixId mod) args) (map (fixStmts mod) stmts)
    P.FnDef i rt n args stmts -> P.FnDef i rt (fixId mod n) (treeMap (fixId mod) args) (map (fixStmts mod) stmts)
    P.StructDef i n args -> P.StructDef i (fixId mod n) (treeMap (fixId mod) args)
    P.TypeDef i n t -> P.TypeDef i (fixId mod n) (fixId mod t)
    u@P.Use {} -> u

fixStmts :: Text -> P.Stmt -> P.Stmt
fixStmts mod = \case
    P.Empty i -> P.Empty i
    P.BStmt i bs -> P.BStmt i (map (fixStmts mod) bs)
    P.Decl i t n -> P.Decl i t (map (treeMap (fixId mod)) n)
    P.Ass i e1 e2 -> P.Ass i (fixExpr mod e1) (fixExpr mod e2)
    P.Incr i id -> P.Incr i (fixId mod id)
    P.Decr i id -> P.Decr i (fixId mod id)
    P.Ret i e -> P.Ret i (fixExpr mod e)
    P.VRet i -> P.VRet i
    P.Cond i e s -> P.Cond i (fixExpr mod e) (fixStmts mod s)
    P.CondElse i e s1 s2 -> P.CondElse i (fixExpr mod e) (fixStmts mod s1) (fixStmts mod s2)
    P.While i e s -> P.While i (fixExpr mod e) (fixStmts mod s)
    P.ForEach i a e s -> P.ForEach i (treeMap (fixId mod) a) (fixExpr mod e) (fixStmts mod s)
    P.Break i -> P.Break i
    P.SExp i e -> P.SExp i (fixExpr mod e)

fixExpr :: Text -> P.Expr -> P.Expr
fixExpr mod = \case
    P.EVar i id -> P.EVar i (fixId mod id)
    P.ELitInt i int -> P.ELitInt i int -- redundant
    P.ELitDouble i d -> P.ELitDouble i d -- redundant
    P.ELitTrue i -> P.ELitTrue i -- redundant
    P.ELitFalse i -> P.ELitFalse i -- redundant
    P.ELitNull i t -> P.ELitNull i t -- redundant
    P.EString i s -> P.EString i s -- redundant
    P.ENew i t s -> P.ENew i t (map (fixExpr mod) s)
    P.EDeref i e id -> P.EDeref i (fixExpr mod e) id
    P.EStructIndex i e ind -> P.EStructIndex i (fixExpr mod e) ind
    P.EIndex i b ind -> P.EIndex i (fixExpr mod b) (fixExpr mod ind)
    P.EApp i b vars -> P.EApp i (fixId mod b) (map (fixExpr mod) vars)
    P.Neg i v -> P.Neg i (fixExpr mod v)
    P.Not i v -> P.Not i (fixExpr mod v)
    P.EMul i lh op rh -> P.EMul i (fixExpr mod lh) op (fixExpr mod rh)
    P.EAdd i lh op rh -> P.EAdd i (fixExpr mod lh) op (fixExpr mod rh)
    P.ERel i lh op rh -> P.ERel i (fixExpr mod lh) op (fixExpr mod rh)
    P.EAnd i lh rh -> P.EAnd i (fixExpr mod lh) (fixExpr mod rh)
    P.EOr i lh rh -> P.EOr i (fixExpr mod lh) (fixExpr mod rh)
