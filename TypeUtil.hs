module TypeUtil where

import Data.List (find)
import Syntax

matchRef :: Refinement -> Refinement -> Bool
matchRef a b = case (a,b) of
  (ValRef (Binding b1 _) _,ValRef (Binding b2 _) _) ->
    b1 == b2
  (DefRef (Binding b1 _) _ _,DefRef (Binding b2 _) _ _) ->
    b1 == b2
  (TypeRef (Binding b1 _) _ _,TypeRef (Binding b2 _) _ _) ->
    b1 == b2
  (MemberRef (Binding b1 _) _ _,TypeRef (Binding b2 _) _ _) ->
    b1 == b2
  _ -> False

mergeRefs :: [Refinement] -> [Refinement] -> [Refinement]
mergeRefs new old = new ++ old'
  where old' = filter search old
        search x = 
          let res = find (matchRef x) new
            in case res of
               Just _  -> False
               Nothing -> True

merge :: Type -> [Refinement] -> Type
merge (Type base rs) rs' = Type base (mergeRefs rs' rs)

--filter for only the MemberRefs
ref :: [Refinement] -> [Refinement]
ref []                       = []
ref (x@(MemberRef _ _ _):xs) = x:ref xs
ref (_:xs)                   = ref xs

--substitution
subst :: Binding -> Path -> Path -> Path
subst x p e = case e of
    Var b -> if b == x then p else Var b
    Field fp fn -> Field (subst x p fp) fn

substType :: Binding -> Path -> Type -> Type
substType x p (Type b rs) = Type (substBaseType x p b) (map (substRefines x p) rs)

substBaseType :: Binding -> Path -> BaseType -> BaseType
substBaseType x p b = case b of
    PathType p' -> PathType (subst x p p')
    orig@_      -> orig

substRefines :: Binding -> Path -> Refinement -> Refinement
substRefines x p r = case r of
    ValRef b ty          -> ValRef b (substType x p ty)
    DefRef b args retTy  -> DefRef b (map (\(Arg bi t) -> Arg bi (substType x p t)) args) (substType x p retTy)
    TypeRef b z rs       -> TypeRef b z (map (substRefines x p) rs)
    MemberRef b bound ty -> MemberRef b bound (substType x p ty)

