{-# LANGUAGE TypeFamilies #-}

module TC.Types where

import Data.Text (Text)

data Tc

data InfoTc = InfoTc
    { sourceLine :: !Int
    , sourceColumn :: !Int
    , sourceName :: !Text
    , sourceCode :: !Text
    , typ :: !TypeTc
    } | NoInfoTc
    deriving (Show, Eq, Ord)

type ExprTc = ExprTc' InfoTc

type AddOpTc = AddOpTc' InfoTc

type ItemTc = ItemTc' InfoTc

type MulOpTc = MulOpTc' InfoTc

type ProgTc = ProgTc' InfoTc

type RelOpTc = RelOpTc' InfoTc

type StmtTc = StmtTc' InfoTc

type TopDefTc = TopDefTc' InfoTc

type TypeTc = TypeTc' InfoTc

type IdTc = IdTc' InfoTc

data ProgTc' a = ProgramTc a [TopDefTc' a]
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data TopDefTc' a = FnDefTc a (TypeTc' a) (IdTc' a) [IdTc' a] [StmtTc' a]
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data ItemTc' a = NoInitTc a (IdTc' a) | InitTc a (IdTc' a) (ExprTc' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data StmtTc' a
    = BStmtTc a [StmtTc' a]
    | DeclTc a [ItemTc' a]
    | AssTc a (IdTc' a) (ExprTc' a)
    | IncrTc a (IdTc' a)
    | DecrTc a (IdTc' a)
    | RetTc a (ExprTc' a)
    | VRetTc a
    | CondTc a (ExprTc' a) (StmtTc' a)
    | CondElseTc a (ExprTc' a) (StmtTc' a) (StmtTc' a)
    | WhileTc a (ExprTc' a) (StmtTc' a)
    | SExpTc a (ExprTc' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data TypeTc' a
    = TVarTc a (IdTc' a)
    | FunTc a (TypeTc' a) [TypeTc' a]
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data ExprTc' a
    = EVarTc a (IdTc' a)
    | ELitIntTc a Integer
    | ELitDoubleTc a Double
    | ELitTrueTc a
    | ELitFalseTc a
    | EAppTc a (IdTc' a) [ExprTc' a]
    | EStringTc a Text
    | NegTc a (ExprTc' a)
    | NotTc a (ExprTc' a)
    | EMulTc a (ExprTc' a) (MulOpTc' a) (ExprTc' a)
    | EAddTc a (ExprTc' a) (AddOpTc' a) (ExprTc' a)
    | ERelTc a (ExprTc' a) (RelOpTc' a) (ExprTc' a)
    | EAndTc a (ExprTc' a) (ExprTc' a)
    | EOrTc a (ExprTc' a) (ExprTc' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data AddOpTc' a
    = PlusTc a
    | MinusTc a
    | AddOpXTc a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data MulOpTc' a
    = TimesTc a
    | DivTc a
    | ModTc a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data RelOpTc' a
    = LTHTc a
    | LETc a
    | GTHTc a
    | GETc a
    | EQUTc a
    | NETc a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data IdTc' a = IdTc a Text
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
