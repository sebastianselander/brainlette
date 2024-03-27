{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BMM.Bmm where

import Data.Kind (Constraint)
import Data.Text (Text)

type family XProgram a

type family XProg a

type ForallProg (f :: * -> Constraint) a =
    ( f (XProgram a)
    , f (XProg a)
    )

newtype Prog a = Program [TopDef a]

deriving instance
    ( ForallTopDef Show a
    , ForallAddOp Show a
    , ForallArg Show a
    , ForallExpr Show a
    , ForallItem Show a
    , ForallMulOp Show a
    , ForallRelOp Show a
    , ForallStmt Show a
    , ForallType Show a
    , ForallProg Show a
    , ForallId Show a
    ) =>
    Show (Prog a)

type family XFnDef a

type family XTopDef a

type ForallTopDef (f :: * -> Constraint) a =
    ( f (XFnDef a)
    , f (XTopDef a)
    )

data TopDef a = FnDef (Type a) (Id a) [Arg a] [Stmt a]

deriving instance
    ( ForallTopDef Show a
    , ForallAddOp Show a
    , ForallArg Show a
    , ForallExpr Show a
    , ForallItem Show a
    , ForallMulOp Show a
    , ForallRelOp Show a
    , ForallStmt Show a
    , ForallType Show a
    , ForallId Show a
    ) =>
    Show (TopDef a)

type family XArgument a

type family XArg a

type ForallArg (f :: * -> Constraint) a =
    ( f (XArgument a)
    , f (XArg a)
    )

data Arg a = Argument (XArgument a) (Type a) (Id a)

deriving instance
    ( ForallArg Show a
    , ForallType Show a
    , ForallId Show a
    ) =>
    Show (Arg a)

type family XInit a

type family XNoInit a

type family XItem a

type ForallItem (f :: * -> Constraint) a =
    ( f (XInit a)
    , f (XNoInit a)
    , f (XItem a)
    )

type family XEmpty a

type family XBStmt a

type family XDecl a

type family XAss a

type family XIncr a

type family XDecr a

type family XRet a

type family XVRet a

type family XCond a

type family XCondElse a

type family XWhile a

type family XSExp a

type family XStmt a

type ForallStmt (f :: * -> Constraint) a =
    ( f (XEmpty a)
    , f (XBStmt a)
    , f (XDecl a)
    , f (XAss a)
    , f (XIncr a)
    , f (XDecr a)
    , f (XRet a)
    , f (XVRet a)
    , f (XCond a)
    , f (XCondElse a)
    , f (XWhile a)
    , f (XSExp a)
    , f (XStmt a)
    )

data Stmt a
    = BStmt (XBStmt a) [Stmt a]
    | Decl (XDecl a) (Type a) (Id a)
    | Ass (XAss a) (Id a) (Expr a)
    | Ret (XRet a) (Maybe (Expr a))
    | CondElse (XCondElse a) (Expr a) (Stmt a) (Stmt a)
    | Loop (XWhile a) (Stmt a)
    | SExp (XSExp a) (Expr a)

deriving instance
    ( ForallStmt Show a
    , ForallType Show a
    , ForallArg Show a
    , ForallItem Show a
    , ForallExpr Show a
    , ForallRelOp Show a
    , ForallAddOp Show a
    , ForallMulOp Show a
    , ForallId Show a
    ) =>
    Show (Stmt a)

type family XTVar a

type family XFun a

type family XType a

type ForallType (f :: * -> Constraint) a =
    ( f (XTVar a)
    , f (XFun a)
    , f (XType a)
    )

data Type a
    = TVar (Id a)
    | Fun (Type a) [Type a]

deriving instance (ForallType Show a, ForallId Show a) => Show (Type a)

type family XEVar a

type family XELitInt a

type family XELitDoub a

type family XELitTrue a

type family XELitFalse a

type family XEApp a

type family XEString a

type family XNeg a

type family XNot a

type family XEMul a

type family XEAdd a

type family XERel a

type family XEAnd a

type family XEOr a

type family XExpr a

type ForallExpr (f :: * -> Constraint) a =
    ( f (XEVar a)
    , f (XELitInt a)
    , f (XELitDoub a)
    , f (XELitTrue a)
    , f (XELitFalse a)
    , f (XEApp a)
    , f (XEString a)
    , f (XNeg a)
    , f (XNot a)
    , f (XEMul a)
    , f (XEAdd a)
    , f (XERel a)
    , f (XEAnd a)
    , f (XEOr a)
    , f (XExpr a)
    )

data Expr a
    = EVar (XEVar a) (Id a)
    | ELitInt (XELitInt a) Integer
    | ELitDouble (XELitDoub a) Double
    | ELitTrue (XELitTrue a)
    | ELitFalse (XELitFalse a)
    | EApp (XEApp a) (Id a) [Expr a]
    | EString (XEString a) Text
    | Neg (XNeg a) (Expr a)
    | Not (XNot a) (Expr a)
    | EMul (XEMul a) (Expr a) (MulOp a) (Expr a)
    | EAdd (XEAdd a) (Expr a) (AddOp a) (Expr a)
    | ERel (XERel a) (Expr a) (RelOp a) (Expr a)
    | EAnd (XEAnd a) (Expr a) (Expr a)
    | EOr (XEOr a) (Expr a) (Expr a)

deriving instance
    ( ForallAddOp Show a
    , ForallMulOp Show a
    , ForallRelOp Show a
    , ForallExpr Show a
    , ForallId Show a
    ) =>
    Show (Expr a)

{- Additive Operator -}

type family XPlus a

type family XMinus a

type family XAddOp a

type ForallAddOp (f :: * -> Constraint) a =
    ( f (XPlus a)
    , f (XMinus a)
    , f (XAddOp a)
    )

data AddOp a
    = Plus (XPlus a)
    | Minus (XMinus a)
    | AddOpX (XAddOp a)

deriving instance (ForallAddOp Show a) => Show (AddOp a)

{- Multiplicative Operator -}

type family XTimes a

type family XDiv a

type family XMod a

type family XMulOp a

type ForallMulOp (f :: * -> Constraint) a =
    ( f (XTimes a)
    , f (XDiv a)
    , f (XMod a)
    , f (XMulOp a)
    )

data MulOp a
    = Times (XTimes a)
    | Div (XDiv a)
    | Mod (XMod a)

deriving instance (ForallMulOp Show a) => Show (MulOp a)

{-  Relational Operator -}

type family XLTH a

type family XLE a

type family XGTH a

type family XGE a

type family XEQU a

type family XNE a

type family XRelOp a

type ForallRelOp (f :: * -> Constraint) a =
    ( f (XLTH a)
    , f (XLE a)
    , f (XGTH a)
    , f (XGE a)
    , f (XEQU a)
    , f (XNE a)
    , f (XRelOp a)
    )

data RelOp a
    = LTH (XLTH a)
    | LE (XLE a)
    | GTH (XGTH a)
    | GE (XGE a)
    | EQU (XEQU a)
    | NE (XNE a)

deriving instance (ForallRelOp Show a) => Show (RelOp a)

-- Identifier

type family XId a

type ForallId (f :: * -> Constraint) a = (f (XId a))

data Id a = Id (XId a) Text

deriving instance (ForallId Show a) => Show (Id a)
