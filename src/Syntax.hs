module Syntax where

type Name = String
data Binding = Binding 
  { name :: Name
  , idx  :: Int
  }
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
  | NamedType Binding
  | PathType Path Name

data Path
  = Var Binding
  | Field Path Name
  deriving (Eq, Ord)

data Expr
  = PathExpr Path
  | Call Path Name [Path]
  | New Type Binding [MemberDefinition]
  | Let Binding Expr Expr
  | TopLit
  | UndefLit

data Bound = LEQ | EQQ | GEQ
  deriving (Eq)
