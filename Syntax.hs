module Syntax where

type Name = String
data Binding = Binding Name Int
  deriving (Eq)
data Arg = Arg Binding Type

data Program = Program [Declaration] Expr

data Declaration = 
    ValDecl Binding Expr
  | DefDecl Binding [Arg] Type Program
  | TypeDecl Binding Binding [Refinement]
  | TypeEqDecl Binding Type
  | SubtypeDecl Type Type

data Type = Type BaseType [Refinement]

data BaseType =
    UnitType
  | BotType
  | PathType Path

theUnit = Type UnitType []
makeNomType s = Type (PathType $ Var s) []

data Refinement =
    ValRef Binding Type
  | DefRef Binding [Arg] Type
  | TypeRef Binding Binding [Refinement]
  | MemberRef Binding Bound Type
  | SubtypeRef Type Type

data Bound = LEQ | EQQ | GEQ
  deriving (Eq)

data Path = 
    Var Binding
  | Field Path Name
  deriving (Eq)

data Expr = 
    PathExpr Path
  | New Type Binding [Declaration]
  | Call Path [Path]
  | IntLit Int
  | UnitLit

subst :: Binding -> Path -> Path -> Path
subst x p e = case e of
    Var b -> if b == x then p else Var b
    Field fp fn -> Field (subst x p fp) fn

substType :: Binding -> Path -> Type -> Type
substType x p (Type b rs) = Type (substBaseType x p b) (map (substRefines x p) rs)

substBaseType :: Binding -> Path -> BaseType -> BaseType
substBaseType x p b = case b of
    PathType p' -> PathType (subst x p p')
    orig@_      -> orig

substRefines :: Binding -> Path -> Refinement -> Refinement
substRefines x p r = case r of
    ValRef b ty          -> ValRef b (substType x p ty)
    DefRef b args retTy  -> DefRef b (map (\(Arg bi t) -> Arg bi (substType x p t)) args) (substType x p retTy)
    TypeRef b z rs       -> TypeRef b z (map (substRefines x p) rs)
    MemberRef b bound ty -> MemberRef b bound (substType x p ty)

