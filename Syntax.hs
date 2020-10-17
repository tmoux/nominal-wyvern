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
