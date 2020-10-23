module Syntax where

type Name = String
data Binding = Binding Name Int
  deriving (Eq, Ord)
data Arg = Arg Binding Type

data TypeAnnot = Shape | Material

data Program = Program [Declaration] Expr

data Declaration = 
    ValDecl Binding (Maybe Type) Expr
  | DefDecl Binding [Arg] Type Program
  | TypeDecl TypeAnnot Binding Binding [Refinement]
  | TypeEqDecl Binding Type
  | SubtypeDecl Type Type --maybe change this to require supertype to be a BaseType

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
  | TypeRef TypeAnnot Binding Binding [Refinement]
  | MemberRef TypeAnnot Binding Bound Type
  | SubtypeRef Type Type

data Bound = LEQ | EQQ | GEQ
  deriving (Eq)

data Path = 
    Var Binding
  | Field Path Name
  deriving (Eq, Ord)

data Expr = 
    PathExpr Path
  | New Type Binding [Declaration]
  | Call Path [Path]
  | IntLit Int
  | UnitLit
