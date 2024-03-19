-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Brainlette.

module Brainlette.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Brainlette.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True , _   , True ) -> []             -- remove trailing space
      (False, _   , True ) -> t              -- remove trailing space
      (False, True, False) -> t ++ ' ' : s   -- add space if none
      _                    -> t ++ s
    where
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Brainlette.Abs.Ident where
  prt _ (Brainlette.Abs.Ident i) = doc $ showString i
instance Print Brainlette.Abs.Prog where
  prt i = \case
    Brainlette.Abs.Program topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print Brainlette.Abs.TopDef where
  prt i = \case
    Brainlette.Abs.FnDef type_ id_ args blk -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 blk])

instance Print [Brainlette.Abs.TopDef] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Brainlette.Abs.Arg where
  prt i = \case
    Brainlette.Abs.Argument type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])

instance Print [Brainlette.Abs.Arg] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Brainlette.Abs.Blk where
  prt i = \case
    Brainlette.Abs.Block stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [Brainlette.Abs.Stmt] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Brainlette.Abs.Stmt where
  prt i = \case
    Brainlette.Abs.Empty -> prPrec i 0 (concatD [doc (showString ";")])
    Brainlette.Abs.BStmt blk -> prPrec i 0 (concatD [prt 0 blk])
    Brainlette.Abs.Decl type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    Brainlette.Abs.Ass id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr, doc (showString ";")])
    Brainlette.Abs.Incr id_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString "++"), doc (showString ";")])
    Brainlette.Abs.Decr id_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString "--"), doc (showString ";")])
    Brainlette.Abs.Ret expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    Brainlette.Abs.VRet -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    Brainlette.Abs.Cond expr stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    Brainlette.Abs.CondElse expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt1, doc (showString "else"), prt 0 stmt2])
    Brainlette.Abs.While expr stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    Brainlette.Abs.SExp expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])

instance Print Brainlette.Abs.Item where
  prt i = \case
    Brainlette.Abs.NoInit id_ -> prPrec i 0 (concatD [prt 0 id_])
    Brainlette.Abs.Init id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])

instance Print [Brainlette.Abs.Item] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Brainlette.Abs.Type where
  prt i = \case
    Brainlette.Abs.Int -> prPrec i 0 (concatD [doc (showString "int")])
    Brainlette.Abs.Doub -> prPrec i 0 (concatD [doc (showString "double")])
    Brainlette.Abs.Bool -> prPrec i 0 (concatD [doc (showString "boolean")])
    Brainlette.Abs.Void -> prPrec i 0 (concatD [doc (showString "void")])
    Brainlette.Abs.Fun type_ types -> prPrec i 0 (concatD [prt 0 type_, doc (showString "("), prt 0 types, doc (showString ")")])

instance Print [Brainlette.Abs.Type] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Brainlette.Abs.Expr where
  prt i = \case
    Brainlette.Abs.EVar id_ -> prPrec i 6 (concatD [prt 0 id_])
    Brainlette.Abs.ELitInt n -> prPrec i 6 (concatD [prt 0 n])
    Brainlette.Abs.ELitDoub d -> prPrec i 6 (concatD [prt 0 d])
    Brainlette.Abs.ELitTrue -> prPrec i 6 (concatD [doc (showString "true")])
    Brainlette.Abs.ELitFalse -> prPrec i 6 (concatD [doc (showString "false")])
    Brainlette.Abs.EApp id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])
    Brainlette.Abs.EString str -> prPrec i 6 (concatD [printString str])
    Brainlette.Abs.Neg expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    Brainlette.Abs.Not expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    Brainlette.Abs.EMul expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    Brainlette.Abs.EAdd expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    Brainlette.Abs.ERel expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    Brainlette.Abs.EAnd expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    Brainlette.Abs.EOr expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])

instance Print [Brainlette.Abs.Expr] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Brainlette.Abs.AddOp where
  prt i = \case
    Brainlette.Abs.Plus -> prPrec i 0 (concatD [doc (showString "+")])
    Brainlette.Abs.Minus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print Brainlette.Abs.MulOp where
  prt i = \case
    Brainlette.Abs.Times -> prPrec i 0 (concatD [doc (showString "*")])
    Brainlette.Abs.Div -> prPrec i 0 (concatD [doc (showString "/")])
    Brainlette.Abs.Mod -> prPrec i 0 (concatD [doc (showString "%")])

instance Print Brainlette.Abs.RelOp where
  prt i = \case
    Brainlette.Abs.LTH -> prPrec i 0 (concatD [doc (showString "<")])
    Brainlette.Abs.LE -> prPrec i 0 (concatD [doc (showString "<=")])
    Brainlette.Abs.GTH -> prPrec i 0 (concatD [doc (showString ">")])
    Brainlette.Abs.GE -> prPrec i 0 (concatD [doc (showString ">=")])
    Brainlette.Abs.EQU -> prPrec i 0 (concatD [doc (showString "==")])
    Brainlette.Abs.NE -> prPrec i 0 (concatD [doc (showString "!=")])
