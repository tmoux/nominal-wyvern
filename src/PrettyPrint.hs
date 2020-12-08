module PrettyPrint where

import Text.Printf
import Syntax
--pretty printing for bound AST
showSep sep [] = ""
showSep sep [x] = show x
showSep sep (x:xs) = (show x) ++ sep ++ (showSep sep xs)

indent s = "  " ++ f s 
  where f [] = ""
        f (x:xs)
          | x == '\n' = [x] ++ "  " ++ f xs
          | otherwise = x:f xs

instance Show Binding where
  show (Binding b t) = b
  --show (Binding b t) = printf "(%s,%s)" b (show t)

instance Show Arg where
  show (Arg b ty) = (show b) ++ ":" ++ (show ty)

instance Show TypeAnnot where
  show Shape    = "@shape "
  show Material = ""

instance Show Program where
  show (Program [] expr) = show expr
  show (Program decls expr) = (showSep "\n" decls) ++ "\n" ++ show expr

instance Show TopLevelDeclaration where
  show (NameDecl ta t z decls) = printf "%stype %s {%s =>\n%s\n}" (show ta) (show t) (show z) (indent $ showSep "\n" decls)
  show (SubtypeDecl t1 t2) = printf "subtype %s extends %s" (show t1) (show t2)
  
instance Show MemberDeclaration where
  show (TypeMemDecl ta t bound ty) = printf "%stype %s %s %s" (show ta) t (show bound) (show ty)
  show (ValDecl v t) = printf "val %s: %s" v (show t)
  show (DefDecl meth args ty) = printf "def %s(%s):%s" meth (showSep ", " args) (show ty)

instance Show MemberDefinition where
  show (TypeMemDefn b t) = printf "type %s = %s" b (show t)
  show (ValDefn b ty e)  = printf "val %s : %s = %s" b (show ty) (show e)
  show (DefDefn b args ty prog) = printf "def %s(%s):%s {\n%s\n}" b (showSep ", " args) (show ty) (indent $ show prog)

instance Show Refinement where
  show (RefineDecl t bound ty) = printf "type %s %s %s" t (show bound) (show ty)

instance Show Type where
  show (Type base []) = show base
  show (Type base refines) = 
    printf "%s {%s}" (show base) (showSep ", " refines)

instance Show BaseType where
  show TopType        = "Top"
  show BotType        = "Bot"
  show (NamedType n)  = show n
  show (PathType p v) = printf "%s.%s" (show p) v

instance Show Path where
  show (Var b) = show b
  show (Field path name) = printf "%s.%s" (show path) name

instance Show Expr where
  show (PathExpr p) = show p
  show (New ty z decls) = printf "new %s {%s =>\n%s\n}" (show ty) (show z) (indent $ showSep "\n" decls)
  show (Call path meth args) = printf "%s.%s(%s)" (show path) meth (showSep ", " args)
  show TopLit   = "Top"
  show (Let x (Just ty) e1 e2) = printf "let %s : %s = %s\nin %s" (show x) (show ty) (show e1) (show e2)
  show (Let x Nothing e1 e2) = printf "let %s = %s\nin %s" (show x) (show e1) (show e2)
  show UndefLit = "undefined"
  show (Assert True t1 t2) = printf "assert %s <: %s" (show t1) (show t2)
  show (Assert False t1 t2) = printf "assertNot %s <: %s" (show t1) (show t2)

instance Show Bound where
  show LEQ = "<="
  show EQQ = "="
  show GEQ = ">="
