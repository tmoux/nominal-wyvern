module TypeUtil where

import Data.List (find)
import Syntax

theUnit = Type UnitType []
theBot  = Type BotType  []
makeNomType s = Type (PathType $ Var s) []

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

(||^) :: Monad m => m Bool -> m Bool -> m Bool
(||^) a b = a >>= (\x -> if x then return True else b)
(&&^) :: Monad m => m Bool -> m Bool -> m Bool
(&&^) a b = a >>= (\x -> if not x then return False else b)
infixr 1 ||^, &&^
m_and :: Monad m => [Bool] -> m Bool
m_and x = return $ and x
m_or  :: Monad m => [Bool] -> m Bool
m_or  x = return $ or x

--monadic bool helper functions

--check that all elements satisfy predicate
checkAll :: Monad m => (a -> m Bool) -> [a] -> m Bool
checkAll f as = (mapM f as) >>= m_and
--check that f is true for all zipped pairs
checkPairwise :: Monad m => (a -> a -> m Bool) -> [a] -> [a] -> m Bool
checkPairwise f as bs = (mapM (uncurry f) (zip as bs)) >>= m_and

--check that for all b in bs, there exists an a such that (f a b) is true
checkPerm :: Monad m => (a -> a -> m Bool) -> [a] -> [a] -> m Bool
checkPerm f as bs = (mapM search bs) >>= m_and 
  where search b = (mapM (flip f b) as) >>= m_or

checkPermDual :: Monad m => (a -> a -> m Bool) -> [a] -> [a] -> m Bool
checkPermDual f as bs = checkPerm f as bs &&^ checkPerm f bs as

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
    SubtypeRef t1 b2 -> SubtypeRef (substType p x t1) (substBaseType p x b2)
