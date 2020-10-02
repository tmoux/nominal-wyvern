module RawSyntax where

type Name = String

data Program = Program [Declaration] Expr
    deriving (Show)

data Declaration = 
    ValDecl Name Expr
  | DefDecl Name [(Name,Type)] Type Program
  | TypeDecl Name Name [Refinement]
  | TypeEq Name Type
  | SubtypeDecl Type Type
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
  | TypeRef Name Name [Refinement]
  | MemberRef Name Bound Type
  | SubtypeRef Type Type
  deriving (Show)

data Bound = LEQ | EQQ | GEQ
  deriving (Show)

data Path = 
    Var Name
  | Field Path Name
  deriving (Show)

data Expr = 
    PathExpr Path
  | New Name Type [Declaration]
  | Call Path [Path]
  | IntLit Int
  | UnitLit
  deriving (Show)
