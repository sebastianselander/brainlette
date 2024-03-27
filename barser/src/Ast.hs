{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ast where

import Data.Text (Text)

data ProgSyn' a = ProgramSyn a [TopDefSyn' a]
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data TopDefSyn' a = FnDefSyn a (TypeSyn' a) (IdSyn' a) [ArgSyn' a] [StmtSyn' a]
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data ArgSyn' a = ArgumentSyn a (TypeSyn' a) (IdSyn' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data ItemSyn' a = NoInitSyn a (IdSyn' a) | InitSyn a (IdSyn' a) (ExprSyn' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data StmtSyn' a
    = EmptySyn a
    | BStmtSyn a [StmtSyn' a]
    | DeclSyn a (TypeSyn' a) [ItemSyn' a]
    | AssSyn a (IdSyn' a) (ExprSyn' a)
    | IncrSyn a (IdSyn' a)
    | DecrSyn a (IdSyn' a)
    | RetSyn a (ExprSyn' a)
    | VRetSyn a
    | CondSyn a (ExprSyn' a) (StmtSyn' a)
    | CondElseSyn a (ExprSyn' a) (StmtSyn' a) (StmtSyn' a)
    | WhileSyn a (ExprSyn' a) (StmtSyn' a)
    | SExpSyn a (ExprSyn' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data TypeSyn' a
    = TVarSyn a (IdSyn' a)
    | FunSyn a (TypeSyn' a) [TypeSyn' a]
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data ExprSyn' a
    = EVarSyn a (IdSyn' a)
    | ELitIntSyn a Integer
    | ELitDoubleSyn a Double
    | ELitTrueSyn a
    | ELitFalseSyn a
    | EAppSyn a (IdSyn' a) [ExprSyn' a]
    | EStringSyn a Text
    | NegSyn a (ExprSyn' a)
    | NotSyn a (ExprSyn' a)
    | EMulSyn a (ExprSyn' a) (MulOpSyn' a) (ExprSyn' a)
    | EAddSyn a (ExprSyn' a) (AddOpSyn' a) (ExprSyn' a)
    | ERelSyn a (ExprSyn' a) (RelOpSyn' a) (ExprSyn' a)
    | EAndSyn a (ExprSyn' a) (ExprSyn' a)
    | EOrSyn a (ExprSyn' a) (ExprSyn' a)
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data AddOpSyn' a
    = PlusSyn a
    | MinusSyn a
    | AddOpXSyn a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data MulOpSyn' a
    = TimesSyn a
    | DivSyn a
    | ModSyn a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data RelOpSyn' a
    = LTHSyn a
    | LESyn a
    | GTHSyn a
    | GESyn a
    | EQUSyn a
    | NESyn a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data IdSyn' a = IdSyn a Text
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
