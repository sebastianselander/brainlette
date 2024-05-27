{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import BMM.TcToBmm (bmm)
import Braingen.Ir (braingen)
import Control.Monad (unless, when)
import Data.Functor (void)
import Data.Text (Text, unlines)
import Data.Text.IO (getContents, hPutStrLn, putStrLn, readFile, writeFile)
import Frontend.BranchReturns (branchCheck)
import Frontend.Error (report)
import Frontend.Parser.BrainletteParser (program)
import Frontend.Renamer (rename)
import Frontend.Tc.Tc (tc)
import Frontend.Uniter (importFiles)
import Lifting.Lifter (lift)
import Options.Applicative
import Runtime (runtime)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName)
import System.IO (stderr)
import System.Process (StdStream (..), createProcess, proc, std_out)
import Utils (Pretty (..), thow)
import Prelude hiding (getContents, putStrLn, readFile, unlines, writeFile)

newtype Options = Options {submissionMode :: Bool}
    deriving (Show)

options :: Parser Options
options = do
    submissionMode <-
        flag
            True
            False
            ( long "normal"
                <> short 'n'
                <> help "print errors"
            )
    return Options {..}

main :: IO ()
main = do
    options <- execParser (info options fullDesc)
    args <- getArgs
    (file, text) <- case args of
        [] -> ("stdin",) <$> getContents
        (file : _) -> do
            b <- doesFileExist file
            unless b $
                ePutStrLn
                    options
                    "brainlette: file does not exist"
                    >> exitFailure
            text <- readFile file
            return (file, text)

    ePutStrLn options "--- Parse output ---"
    res <- case program file text of
        Left err -> errorExit options (thow err)
        Right res -> return res

    ePutStrLn options "--- Files to import ---"
    res <- importFiles res
    res <- case res of
        Left err -> errorExit options (report err)
        Right res -> return res
    ePutStrLn options (pretty 0 res)

    ePutStrLn options "--- Renamer output ---"
    (res, _) <- case rename res of
        Left err -> errorExit options err
        Right res -> return res

    ePutStrLn options (pretty 0 res)
    res <- case branchCheck res of
        Left err -> errorExit options err
        Right res -> return res

    ePutStrLn options "\n--- Branch check output ---"

    ePutStrLn options (pretty 0 res)

    res <- case tc res of
        Left err -> errorExit options err
        Right res -> return res

    ePutStrLn options "\n--- Typecheck output ---"
    ePutStrLn options (pretty 0 res)

    ePutStrLn options "\n--- Lifter output ---"

    (res, lifteds) <- return (lift res)
    ePutStrLn options (pretty 0 res)

    ePutStrLn options "\n--- BMM output ---"

    (res, funs) <- return $ bmm res
    ePutStrLn options (pretty 0 res)
    -- writeFile "output.bmm" (pretty 0 res)

    res <- return $ braingen res lifteds funs

    ePutStrLn options "\n--- LLVM IR output ---"
    let res' = unlines [runtime options.submissionMode, res]
    ePutStrLn options res'

    writeFile "output.ll" res'
    if options.submissionMode
        then pure ()
        else
            void $
                createProcess $
                    ( proc
                        "clang"
                        [ "-O3"
                        , "output.ll"
                        , "-o"
                        , takeBaseName file
                        ]
                    )
                        { std_out = UseHandle stderr
                        }
    putStrLn res'
    ok options

-- | Print string to stdErr
ePutStrLn :: Options -> Text -> IO ()
ePutStrLn o txt = if o.submissionMode then pure () else hPutStrLn stderr txt

-- | Print error and exit
errorExit :: forall a. Options -> Text -> IO a
errorExit o err =
    if o.submissionMode
        then hPutStrLn stderr "ERROR" *> exitFailure
        else hPutStrLn stderr err *> exitFailure

-- | Print OK
ok :: Options -> IO ()
ok o = when o.submissionMode $ hPutStrLn stderr "OK"
