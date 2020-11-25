module RawSyntax where

type Name = String

data TypeAnnot = Shape | Material
  deriving (Show)

data Program = Program [Declaration] Expr
    deriving (Show)

data Declaration = 
    --ValDecl Name Expr | 
    ValAnnotDecl Name Type Expr
  | DefDecl Name [(Name,Type)] Type Program
  | TypeDecl TypeAnnot Name Name [Refinement]
  | TypeEqDecl Name Type
  | SubtypeDecl Type BaseType
  deriving (Show)

data Type = Type BaseType [Refinement]
    deriving (Show)

data BaseType =
    UnitType
  | BotType
  | PathType Path
  deriving (Show)

data Refinement =
    ValRef Name Type
  | DefRef Name [(Name,Type)] Type
  | TypeRef TypeAnnot Name Name [Refinement]
  | MemberRef TypeAnnot Name Bound Type
  | SubtypeRef Type BaseType
  deriving (Show)

data Bound = LEQ | EQQ | GEQ
  deriving (Show)

data Path = 
    Var Name
  | Field Path Name
  deriving (Show)

data Expr = 
    PathExpr Path
  | New Type Name [Declaration]
  | Call Path [Path]
  | IntLit Int
  | UnitLit
  | UndefLit
  deriving (Show)
