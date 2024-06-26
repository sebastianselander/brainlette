{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Utils where

import Data.Generics (mkQ, everywhere)
import Data.Maybe (maybeToList)
import Data.Text (Text, intercalate, pack, replicate)
import GHC.Stack (HasCallStack)
import Generics.SYB
    ( Data,
      Typeable,
      everything,
      everywhereM,
      mkM,
      mkT,
      Data,
      Typeable,
      everywhereM,
      mkM,
      Data,
      Typeable,
      everywhereM,
      mkM,
      mkT )
import Prelude hiding (replicate)

{-# WARNING TODO "TODO" #-}
pattern TODO :: a
pattern TODO <- _
    where
        TODO = error "TODO: Not yet implemented"

{-# WARNING todo "todo" #-}
todo :: (HasCallStack) => a
todo = error "TODO: Not yet implemented"

treeMapM :: (Data a, Monad m, Typeable b) => (b -> m b) -> a -> m a
treeMapM f = everywhereM (mkM f)

treeMap :: (Data a, Typeable b) => (b -> b) -> a -> a
treeMap f = everywhere (mkT f)

-- | map with flipped arguments
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | concatMap with flipped arguments
concatFor :: [a] -> (a -> [b]) -> [b]
concatFor = flip concatMap

-- | Show but text :)
thow :: (Show a) => a -> Text
thow = pack . show

flat3 :: (a, (b, c)) -> (a, b, c)
flat3 (a, (b, c)) = (a, b, c)

class Pretty a where
    {-# MINIMAL pretty #-}
    pretty :: Int -> a -> Text

    parenthesis :: Int -> a -> Text
    parenthesis n a = "(" <> pretty n a <> ")"

    indent :: Int -> a -> Text
    indent n a = replicate (n * 4) " " <> pretty n a

    commaSeparated :: Int -> [a] -> Text
    commaSeparated n = intercalate ", " . map (pretty n)

    semi :: Int -> a -> Text
    semi n a = pretty n a <> ";"

apN :: Int -> (a -> a) -> a -> a
apN 0 _ x = x
apN !n f !x = apN (n - 1) f (f x)

instance Pretty Text where
    pretty _ t = t

pured :: (Functor f, Applicative t) => f a -> f (t a)
pured thing = pure <$> thing

listify' :: (Data a, Typeable b) => (b -> Maybe c) -> a -> [c]
listify' f = everything (++) ([] `mkQ` (maybeToList . f))

compilerPrims :: [Text]
compilerPrims =
    [ "printInt"
    , "printString"
    , "printDouble"
    , "readInt"
    , "readDouble"
    , "readString"
    ]


type Toplevel = Bool
