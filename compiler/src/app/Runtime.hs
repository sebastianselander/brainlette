{-# LANGUAGE TemplateHaskell #-}

module Runtime (readRuntime) where

import Language.Haskell.TH

readRuntime :: FilePath -> Q Exp
readRuntime fp = LitE . StringL <$> runIO (readFile fp)
