module Frontend.Parser.BrainletteParser
    ( arg,
      expr,
      program,
      stmt,
      topdef,
      typ,
      HasInfo (..),
    )
where

import Frontend.Parser.ArgumentParser (arg)
import Frontend.Parser.ExprParser (expr)
import Frontend.Parser.ParserTypes
import Frontend.Parser.ProgramParser (program)
import Frontend.Parser.StmtParser (stmt)
import Frontend.Parser.TopDefParser (topdef)
import Frontend.Parser.TypeParser (typ)

class HasInfo a where
    hasInfo :: a -> SynInfo

instance HasInfo Id where
    hasInfo (Id a _ _) = a

instance HasInfo RelOp where
    hasInfo (LTH a) = a
    hasInfo (LE a) = a
    hasInfo (GTH a) = a
    hasInfo (GE a) = a
    hasInfo (EQU a) = a
    hasInfo (NE a) = a

instance HasInfo MulOp where
    hasInfo (Times a) = a
    hasInfo (Div a) = a
    hasInfo (Mod a) = a

instance HasInfo AddOp where
    hasInfo (Plus a) = a
    hasInfo (Minus a) = a

instance HasInfo Expr where
    hasInfo (EVar a _) = a
    hasInfo (ELitInt a _) = a
    hasInfo (ELitDouble a _) = a
    hasInfo (ELitTrue a) = a
    hasInfo (ELitFalse a) = a
    hasInfo (ELitNull a _) = a
    hasInfo (EString a _) = a
    hasInfo (EApp a _ _) = a
    hasInfo (EDeref a _ _) = a
    hasInfo (Neg a _) = a
    hasInfo (Not a _) = a
    hasInfo (EMul a _ _ _) = a
    hasInfo (EAdd a _ _ _) = a
    hasInfo (ERel a _ _ _) = a
    hasInfo (EAnd a _ _) = a
    hasInfo (EOr a _ _) = a
    hasInfo (ENew a _ _) = a
    hasInfo (EIndex a _ _) = a
    hasInfo (EStructIndex a _ _) = a
    hasInfo (ELam a _ _ _) = a

instance HasInfo Type where
    hasInfo (TVar a _) = a
    hasInfo (Fun a _ _) = a
    hasInfo (String a) = a
    hasInfo (Int a) = a
    hasInfo (Double a) = a
    hasInfo (Void a) = a
    hasInfo (Boolean a) = a
    hasInfo (Pointer a _) = a
    hasInfo (Array a _) = a

instance HasInfo Stmt where
    hasInfo (Empty a) = a
    hasInfo (BStmt a _) = a
    hasInfo (Decl a _ _) = a
    hasInfo (Ass a _ _) = a
    hasInfo (Incr a _) = a
    hasInfo (Decr a _) = a
    hasInfo (Ret a _) = a
    hasInfo (VRet a) = a
    hasInfo (Cond a _ _) = a
    hasInfo (CondElse a _ _ _) = a
    hasInfo (While a _ _) = a
    hasInfo (SExp a _) = a
    hasInfo (Break a) = a
    hasInfo (ForEach a _ _ _) = a
    hasInfo (SFn _ func) = hasInfo func
    hasInfo (ForI a _ _ _ _) = a

instance HasInfo Item where
    hasInfo (NoInit a _) = a
    hasInfo (Init a _ _) = a

instance HasInfo Arg where
    hasInfo (Argument a _ _) = a

instance HasInfo TopDef where
    hasInfo (FnDef _ func) = hasInfo func
    hasInfo (StructDef a _ _) = a
    hasInfo (TypeDef a _ _) = a
    hasInfo (Use a _) = a

instance HasInfo Prog where
    hasInfo (Program a _) = a

instance HasInfo Function where
    hasInfo (Fn a _ _ _ _) = a

