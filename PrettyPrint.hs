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

instance Show Arg where
    show (Arg b ty) = (show b) ++ ":" ++ (show ty)

instance Show Program where
    show (Program [] expr) = show expr
    show (Program decls expr) =
        (showSep "\n" decls) ++ "\n" ++ show expr

instance Show Declaration where
  show d = case d of
    ValDecl b e -> 
      printf "val %s = %s" (show b) (show e)
    DefDecl b args ty prog ->
      printf "def %s(%s):%s {\n%s\n}" (show b) (showSep ", " args) (show ty) (indent $ show prog)
    TypeDecl t z refines ->
      printf "type %s {%s =>\n%s\n}" (show t) (show z) (indent $ showSep "\n" refines)
    TypeEqDecl b t ->
      printf "type %s = %s" (show b) (show t)
    SubtypeDecl t1 t2 ->
      printf "subtype %s extends %s" (show t1) (show t2)

instance Show Refinement where
  show r = case r of
    ValRef b t ->
      printf "val %s: %s" (show b) (show t)
    DefRef b args ty ->
      printf "def %s(%s):%s" (show b) (showSep ", " args) (show ty)
    TypeRef t z refines ->
      printf "type %s {%s =>\n%s\n}" (show t) (show z) (indent $ showSep "\n" refines)
    MemberRef b bound ty ->
      printf "type %s %s %s" (show b) (show bound) (show ty)
    SubtypeRef t1 t2 ->
      printf "subtype %s extends %s" (show t1) (show t2)

instance Show Bound where
  show LEQ = "<="
  show EQQ = "="
  show GEQ = ">="

instance Show Path where
  show p = case p of
    Var b -> show b
    Field path name ->
      printf "%s.%s" (show path) name

instance Show Expr where
  show e = case e of
    PathExpr p -> show p
    New ty b decls ->
      printf "new %s {%s =>\n%s\n}" (show ty) (show b) (indent $ showSep "\n" decls)  
    Call meth args ->
      printf "%s(%s)" (show meth) (showSep ", " args)
    IntLit i -> show i
    UnitLit -> "Unit"

instance Show Type where
  show (Type base []) = show base
  show (Type base refines) = 
    printf "%s {%s}" (show base) (showSep ", " refines)

instance Show BaseType where
  show b = case b of
    UnitType   -> "Unit"
    BotType    -> "Bot"
    PathType p -> show p
