module TypeUtil where

import Data.List (find)
import Syntax

matchRef :: Refinement -> Refinement -> Bool
matchRef a b = case (a,b) of
  (ValRef (Binding b1 _) _,ValRef (Binding b2 _) _) ->
    b1 == b2
  (DefRef (Binding b1 _) _ _,DefRef (Binding b2 _) _ _) ->
    b1 == b2
  (TypeRef _ (Binding b1 _) _ _,TypeRef _ (Binding b2 _) _ _) ->
    b1 == b2
  (MemberRef _ (Binding b1 _) _ _,MemberRef _ (Binding b2 _) _ _) ->
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
ref (x@(MemberRef _ _ _ _):xs) = x:ref xs
ref (_:xs)                   = ref xs

--substitution
--substitute PATH for BINDING (p/x) in [path/type/refinement]
subst :: Path -> Binding -> Path -> Path
subst p x e = case e of
    Var b -> if b == x then p else Var b
    Field fp fn -> Field (subst p x fp) fn

substType :: Path -> Binding -> Type -> Type
substType p x (Type b rs) = Type (substBaseType p x b) (map (substRefines p x) rs)

substBaseType :: Path -> Binding -> BaseType -> BaseType
substBaseType p x base = case base of
    PathType p' -> PathType (subst p x p')
    _           -> base

substRefines :: Path -> Binding -> Refinement -> Refinement
substRefines p x r = case r of
    ValRef b ty          -> ValRef b (substType p x ty)
    DefRef b args retTy  -> DefRef b (map (\(Arg bi t) -> Arg bi (substType p x t)) args) (substType p x retTy)
    TypeRef ta b z rs       -> TypeRef ta b z (map (substRefines p x) rs)
    MemberRef ta b bound ty -> MemberRef ta b bound (substType p x ty)
