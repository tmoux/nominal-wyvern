module Binding where 

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (find)
import qualified RawSyntax as Raw
import Syntax

data BindCtx = BindVal Binding
             | BindType Binding

type BindMonad = ReaderT [BindCtx] (StateT Int (Except String))

newBinding :: Name -> BindMonad Binding
newBinding name = do
    cnt <- get
    put (cnt+1)
    return $ Binding name cnt

fetchVal :: Name -> BindMonad Binding
fetchVal v = do
  ctx <- ask
  case (find pred ctx) of
      Just (BindVal b) -> return b
      Just _ -> throwError "shouldn't happen"
      Nothing -> throwError $ "binding lookup failed: " ++ v
  where pred (BindVal b) = name b == v
        pred _ = False

fetchType :: Name -> BindMonad Binding
fetchType n = do
  ctx <- ask
  case (find pred ctx) of
      Just (BindType b) -> return b
      Just _ -> throwError "shouldn't happen"
      Nothing -> throwError $ "binding lookup failed: " ++ n
  where pred (BindType b) = name b == n
        pred _ = False

convertTA Raw.Shape    = Shape
convertTA Raw.Material = Material

convertBound Raw.LEQ = LEQ
convertBound Raw.EQQ = EQQ
convertBound Raw.GEQ = GEQ

bind prog = runExcept (evalStateT (
              runReaderT (bindProgram prog) []
            ) 0)

bindProgram :: Raw.Program -> BindMonad Program
bindProgram (Raw.Program decls expr) = do
  names <- f decls
  local (names++) $ do
    decls' <- mapM bindTLDecl decls
    expr' <- bindExpr expr
    return (Program decls' expr')
  where f [] = return []
        f (Raw.NameDecl _ n _ _:xs) = do
          n' <- newBinding n
          xs' <- f xs
          return (BindType n':xs')
        f (_:xs) = f xs

bindTLDecl :: Raw.TopLevelDeclaration -> BindMonad TopLevelDeclaration
bindTLDecl d = case d of
  Raw.NameDecl ta n z decls -> do
    let ta' = convertTA ta
    n' <- fetchType n
    z' <- newBinding z
    decls' <- local ([BindVal z',BindType n']++) $ mapM bindMemberDecl decls
    return $ NameDecl ta' n' z' decls'
  Raw.SubtypeDecl t1 t2 -> do
    t1' <- bindType t1
    t2' <- bindBaseType t2
    return $ SubtypeDecl t1' t2'

bindMemberDecl :: Raw.MemberDeclaration -> BindMonad MemberDeclaration
bindMemberDecl r = case r of
  Raw.TypeMemDecl ta b bound ty -> do
    let ta' = convertTA ta
    let bound' = convertBound bound
    ty' <- bindType ty
    return $ TypeMemDecl ta' b bound' ty'
  Raw.ValDecl b ty -> do
    ty' <- bindType ty
    return $ ValDecl b ty'
  Raw.DefDecl b args ty -> do
    let bindArgs [] ty = do
          ty' <- bindType ty
          return ([],ty')
        bindArgs ((n,t):xs) ty = do
          n' <- newBinding n
          t' <- bindType t
          (ns,ty') <- local (BindVal n':) $ bindArgs xs ty
          return ((Arg n' t':ns),ty')
    (args',ty') <- bindArgs args ty
    return $ DefDecl b args' ty'

bindMemberDefn :: Raw.MemberDefinition -> BindMonad MemberDefinition
bindMemberDefn d = case d of
  Raw.TypeMemDefn b ty -> do
    ty' <- bindType ty
    return $ TypeMemDefn b ty'
  Raw.ValDefn b ty e -> do
    ty' <- bindType ty
    e'  <- bindExpr e
    return $ ValDefn b ty' e'
  Raw.DefDefn b args ty e -> do
    let bindArgs [] expr ty = do
          e'  <- bindExpr expr
          ty' <- bindType ty
          return ([],e',ty')
        bindArgs ((n,t):xs) expr ty = do
          n' <- newBinding n  
          t' <- bindType t
          (ns,e',ty') <- local (BindVal n':) $ bindArgs xs expr ty
          return (Arg n' t':ns,e',ty')
    (args',e',ty') <- bindArgs args e ty
    return $ DefDefn b args' ty' e'
    
bindRefinement :: Raw.Refinement -> BindMonad Refinement
bindRefinement (Raw.RefineDecl t bound ty) = do
  let bound' = convertBound bound 
  ty' <- bindType ty
  return $ RefineDecl t bound' ty'

bindMaybeType :: Maybe Raw.Type -> BindMonad (Maybe Type)
bindMaybeType (Just ty) = do
  ty' <- bindType ty
  return (Just ty')
bindMaybeType Nothing = return Nothing

bindType :: Raw.Type -> BindMonad Type
bindType (Raw.Type b rs) = do
  b' <- bindBaseType b
  rs' <- mapM bindRefinement rs
  return $ Type b' rs'

bindBaseType :: Raw.BaseType -> BindMonad BaseType
bindBaseType b = case b of
  Raw.TopType -> return TopType
  Raw.BotType -> return BotType
  Raw.NamedType n -> do
    n' <- fetchType n
    return $ NamedType n'
  Raw.PathType p t -> do
    p' <- bindPath p
    return $ PathType p' t

bindExpr :: Raw.Expr -> BindMonad Expr
bindExpr e = case e of
  Raw.PathExpr p -> do
    p' <- bindPath p
    return $ PathExpr p'
  Raw.Call path meth args -> do
    path' <- bindPath path
    args' <- mapM bindPath args
    return $ Call path' meth args'
  Raw.New ty name defns -> do
    ty'    <- bindType ty
    b      <- newBinding name
    defns' <- local ((BindVal b):) $ mapM bindMemberDefn defns
    return $ New ty' b defns'
  Raw.Let x annot e1 e2 -> do
    x' <- newBinding x
    e1' <- bindExpr e1
    e2' <- local (BindVal x':) $ bindExpr e2
    annot' <- bindMaybeType annot
    return $ Let x' annot' e1' e2'
  Raw.IntLit i -> do
    intTy <- bindType (Raw.Type (Raw.NamedType "Int") [])
    z <- newBinding "z"
    a <- newBinding "a"
    let defns = [DefDefn "plus" [Arg a intTy] intTy UndefLit]
    return $ New intTy z defns
  Raw.TopLit -> return TopLit  
  Raw.UndefLit -> return UndefLit

bindPath :: Raw.Path -> BindMonad Path
bindPath p = case p of
  Raw.Var v -> do
    b <- fetchVal v
    return $ Var b
  Raw.Field path name -> do
    path' <- bindPath path
    return $ Field path' name
