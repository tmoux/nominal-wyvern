module RawSyntax where

type Name = String

data TypeAnnot = Shape | Material
  deriving (Show)

data Program = Program [TopLevelDeclaration] Expr
    deriving (Show)

data TopLevelDeclaration
  = NameDecl TypeAnnot Name Name [MemberDeclaration]
  | SubtypeDecl Type BaseType
  deriving (Show)

data MemberDeclaration
  = TypeMemDecl TypeAnnot Name Bound Type
  | ValDecl Name Type
  | DefDecl Name [(Name,Type)] Type
  deriving (Show)

data MemberDefinition
  = TypeMemDefn Name Type
  | ValDefn Name Type Expr
  | DefDefn Name [(Name,Type)] Type Expr
  deriving (Show)

data Refinement = RefineDecl Name Bound Type
  deriving (Show)

data Type = Type BaseType [Refinement]
    deriving (Show)

data BaseType
  = TopType
  | BotType
  | NamedType Name
  | PathType Path Name
  deriving (Show)

data Path
  = Var Name
  | Field Path Name
  deriving (Show)

data Expr
  = PathExpr Path
  | Call Path Name [Path]
  | New Type Name [MemberDefinition]
  | Let Name (Maybe Type) Expr Expr
  | IntLit Int
  | TopLit
  | UndefLit
  deriving (Show)

data Bound = LEQ | EQQ | GEQ
  deriving (Show)
