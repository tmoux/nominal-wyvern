module Syntax where

type Name = String
data Binding = Binding 
  { name :: Name
  , idx  :: Int
  }
  deriving (Eq, Ord)
data Arg = Arg
  { argName :: Binding
  , argType :: Type
  }

data TypeAnnot = Shape | Material

data Program = Program [TopLevelDeclaration] Expr

data TopLevelDeclaration
  = NameDecl TypeAnnot Binding Binding [MemberDeclaration]
  | SubtypeDecl Type BaseType

data MemberDeclaration
  = TypeMemDecl TypeAnnot Name Bound Type
  | ValDecl Name Type
  | DefDecl Name [Arg] Type

data MemberDefinition
  = TypeMemDefn Name Type
  | ValDefn Name Type Expr
  | DefDefn Name [Arg] Type Expr

data Refinement = RefineDecl Name Bound Type

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
  | Let Binding (Maybe Type) Expr Expr
  | TopLit
  | UndefLit
  | Assert Bool Type Type

data Bound = LEQ | EQQ | GEQ
  deriving (Eq)
