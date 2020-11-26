module Syntax where

type Name = String
data Binding = Binding Name Int
  deriving (Eq, Ord)
data Arg = Arg Binding Type

data TypeAnnot = Shape | Material

data Program = Program [TopLevelDeclaration] Expr

data TopLevelDeclaration
  = NameDecl TypeAnnot Binding Binding [MemberDeclaration]
  | SubtypeDecl Type BaseType

data MemberDeclaration
  = TypeMemDecl TypeAnnot Binding Bound Type
  | ValDecl Binding Type
  | DefDecl Binding [Arg] Type

data MemberDefinition
  = TypeMemDefn Binding Type
  | ValDefn Binding Type Expr
  | DefDefn Binding [Arg] Type Expr

data Refinement = RefineDecl Binding Bound Type

data Type = Type BaseType [Refinement]

data BaseType
  = TopType
  | BotType
  | PathType Path

data Bound = LEQ | EQQ | GEQ
  deriving (Eq)

data Path
  = Var Binding
  | Field Path Name
  deriving (Eq, Ord)

data Expr
  = PathExpr Path
  | Call Path [Path]
  | New Type Binding [MemberDeclaration]
  | Let Binding Expr Expr
  | TopLit
  | UndefLit
