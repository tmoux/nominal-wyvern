module Syntax where

type Name = String
type Binding = (Name,Int)

data Program = Program [Declaration] Expr
    deriving (Show)

data Declaration = 
    ValDecl Binding Expr
  | DefDecl Binding [(Binding,Type)] Type Program
  | TypeDecl Binding Binding [Refinement]
  | TypeEqDecl Binding Type
  | SubtypeDecl Type Type
  deriving (Show)

data Type = Type BaseType [Refinement]
    deriving (Show)

data BaseType =
    UnitType
  | BotType
  | PathType Path
  deriving (Show)

theUnit = Type UnitType []
makeNomType s = Type (PathType $ Var s) []

data Refinement =
    ValRef Binding Type
  | DefRef Binding [(Binding,Type)] Type
  | TypeRef Binding Binding [Refinement]
  | MemberRef Binding Bound Type
  | SubtypeRef Type Type
  deriving (Show)

data Bound = LEQ | EQQ | GEQ
  deriving (Show)

data Path = 
    Var Binding
  | Field Path Name
  deriving (Show)

data Expr = 
    PathExpr Path
  | New Binding Type [Declaration]
  | Call Path [Path]
  | IntLit Int
  | UnitLit
  deriving (Show)

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
    DefRef b args retTy  -> DefRef b (map (\(bi,t) -> (bi,substType x p t)) args) (substType x p retTy)
    TypeRef b z rs       -> TypeRef b z (map (substRefines x p) rs)
    MemberRef b bound ty -> MemberRef b bound (substType x p ty)
