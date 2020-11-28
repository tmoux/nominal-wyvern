module TypeUtil where

import Control.Monad.Extra
import Data.List (find)
import Data.Maybe (isNothing)
import Syntax

theTop = Type TopType []
theBot = Type BotType  []
makeNomType s = Type (NamedType s) []

argToDecl :: Arg -> MemberDeclaration
argToDecl (Arg x ty) = ValDecl x ty

sameName :: Binding -> Binding -> Bool
sameName b1 b2 = name b1 == name b2

refToDecl :: Refinement -> MemberDeclaration
refToDecl (RefineDecl t bound ty) = TypeMemDecl Material t bound ty

matchRef :: Refinement -> Refinement -> Bool
matchRef (RefineDecl t1 _ _) (RefineDecl t2 _ _) = sameName t1 t2

mergeRefs :: [Refinement] -> [Refinement] -> [Refinement]
mergeRefs new old = new ++ old'
  where old'     = filter search old
        search x = isNothing $ find (matchRef x) new

merge :: Type -> [Refinement] -> Type
merge (Type base rs) rs' = Type base (mergeRefs rs' rs)

--turn definitions into member declarations
sig :: [MemberDefinition] -> [MemberDeclaration]
sig = map f
  where f (TypeMemDefn t ty) = TypeMemDecl Material t EQQ ty
        f (ValDefn v ty _) = ValDecl v ty
        f (DefDefn meth args ty _) = DefDecl meth args ty
--filter for only the type members and turn them into refinements
ref :: [MemberDeclaration] -> [Refinement]
ref [] = []
ref (TypeMemDecl _ t b ty:xs) = RefineDecl t b ty:ref xs
ref (_:xs) = ref xs

--check that all elements satisfy predicate
checkAll :: Monad m => (a -> m Bool) -> [a] -> m Bool
checkAll = allM
--check that f is true for all zipped pairs
checkPairwise :: Monad m => (a -> a -> m Bool) -> [a] -> [a] -> m Bool
checkPairwise f as bs = allM (uncurry f) (zip as bs)

m_and :: Monad m => [Bool] -> m Bool
m_and x = return $ and x
m_or  :: Monad m => [Bool] -> m Bool
m_or  x = return $ or x
--check that for all b in bs, there exists an a such that (f a b) is true
checkPerm :: Monad m => (a -> a -> m Bool) -> [a] -> [a] -> m Bool
checkPerm f as bs = allM (\b -> anyM (flip f b) as) bs

checkPermDual :: Monad m => (a -> a -> m Bool) -> [a] -> [a] -> m Bool
checkPermDual f as bs = checkPerm f as bs &&^ checkPerm f bs as

--substitution
--substitute PATH for BINDING (p/x) in [path/type/member decl]
class Substitute a where
  subst :: Path -> Binding -> a -> a

instance Substitute Path where
  subst p x e = case e of
    Var b -> if b == x then p else Var b
    Field fp fn -> Field (subst p x fp) fn

instance Substitute Type where
  subst p x (Type b rs) = Type (subst p x b) (map (subst p x) rs)

instance Substitute BaseType where
  subst p x base = case base of
    PathType p' t -> PathType (subst p x p') t
    _             -> base

instance Substitute MemberDeclaration where
  subst p x d = case d of
    TypeMemDecl ta b bound ty -> TypeMemDecl ta b bound (subst p x ty)
    ValDecl b ty          -> ValDecl b (subst p x ty)
    DefDecl b args retTy  -> DefDecl b (map (\(Arg bi t) -> Arg bi (subst p x t)) args) (subst p x retTy)

instance Substitute Refinement where
  subst p x (RefineDecl t bound ty) = RefineDecl t bound (subst p x ty)

instance (Substitute a) => Substitute [a] where
  subst p x list = map (subst p x) list 
