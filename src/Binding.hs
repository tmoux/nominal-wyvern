module Binding where 

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (find)
import qualified RawSyntax as Raw
import Syntax

data Error = OtherErr String
    deriving (Show)

data BindCtx = BindVal Binding
             | BindType Binding
             | BindDef Binding

type BindMonad = ReaderT [BindCtx] (StateT Int (Except String))

newBinding :: Name -> BindMonad Binding
newBinding name = do
    cnt <- get
    put (cnt+1)
    return $ Binding name cnt

--moar code duplication
fetchVal :: Name -> BindMonad Binding
fetchVal name = do
    ctx <- ask
    case (find pred ctx) of
        Just (BindVal b) -> return b
        Just _ -> throwError "shouldn't happen"
        Nothing -> throwError $ "binding lookup failed: " ++ name
    where pred (BindVal (Binding b _)) = b == name
          pred _ = False

fetchType :: Name -> BindMonad Binding
fetchType name = do
    ctx <- ask
    case (find pred ctx) of
        Just (BindType b) -> return b
        Just _ -> throwError "shouldn't happen"
        Nothing -> throwError $ "binding lookup failed: " ++ name
    where pred (BindType (Binding b _)) = b == name
          pred _ = False

fetchDef :: Name -> BindMonad Binding
fetchDef name = do
    ctx <- ask
    case (find pred ctx) of
        Just (BindDef b) -> return b
        Just _ -> throwError "shouldn't happen"
        Nothing -> throwError $ "binding lookup failed: " ++ name
    where pred (BindDef (Binding b _)) = b == name
          pred _ = False

toBindCtx :: Declaration -> Maybe BindCtx
toBindCtx d = case d of
  ValDecl b _ _    -> Just $ BindVal b
  TypeDecl _ b _ _ -> Just $ BindType b
  DefDecl b _ _ _  -> Just $ BindDef b
  _ -> Nothing

convertTA Raw.Shape    = Shape
convertTA Raw.Material = Material

bind prog = runExcept (evalStateT (
              runReaderT (bindProgram prog) []
            ) 0)

--bindSingleDecl decl ctx cnt = evalState (
--                                runReaderT (
--                                  runExceptT (bindDecl decl)
--                                ) ctx
--                              ) cnt

bindProgram :: Raw.Program -> BindMonad Program
bindProgram (Raw.Program decls expr) = f decls expr
    where f [] expr = do
            e <- bindExpr expr
            return $ Program [] e
          f (x:xs) expr = do
            d <- bindDecl x
            let ctxOp = case (toBindCtx d) of
                          Just b  -> (b:)
                          Nothing -> id
            Program ds e <- local ctxOp $ f xs expr
            return $ Program (d:ds) e

bindDecl :: Raw.Declaration -> BindMonad Declaration
bindDecl d = case d of
  --Raw.ValDecl b e -> do  
  --  b' <- newBinding b   
  --  e' <- bindExpr e
  --  return $ ValDecl b' Nothing e'
  Raw.ValAnnotDecl b ty e -> do
    b'  <- newBinding b
    ty' <- bindType ty
    e'  <- bindExpr e
    return $ ValDecl b' ty' e'
  Raw.DefDecl b args ty prog -> do
    b' <- newBinding b
    let bindArgs [] prog ty = do
         p'  <- bindProgram prog
         ty' <- bindType ty
         return ([],p',ty')
        bindArgs ((n,t):xs) prog ty = do
          n' <- newBinding n  
          t' <- bindType t
          (ns,p',ty') <- local (BindVal n':) $ bindArgs xs prog ty
          return (Arg n' t':ns,p',ty')
    (args',prog',ty') <- bindArgs args prog ty
    return $ DefDecl b' args' ty' prog'
  Raw.TypeDecl ta b z refines -> do
    let ta' = convertTA ta
    b' <- newBinding b
    z' <- newBinding z
    let tType = BindType b'
    let zVar = BindVal z'    
    refines' <- local ([zVar,tType] ++) $ mapM bindRefine refines
    return $ TypeDecl ta' b' z' refines'
  Raw.TypeEqDecl b ty -> do
    b' <- newBinding b
    ty' <- bindType ty
    return $ TypeEqDecl b' ty'
  Raw.SubtypeDecl t1 t2 -> do
    t1' <- bindType t1
    t2' <- bindBaseType t2
    return $ SubtypeDecl t1' t2'

bindRefine :: Raw.Refinement -> BindMonad Refinement
bindRefine r = case r of
  Raw.ValRef b ty -> do
    b'  <- newBinding b
    ty' <- bindType ty
    return $ ValRef b' ty'
  Raw.DefRef b args ty -> do
    b' <- newBinding b
    let bindArgs [] ty = do
          ty' <- bindType ty
          return ([],ty')
        bindArgs ((n,t):xs) ty = do
          n' <- newBinding n
          t' <- bindType t
          (ns,ty') <- local (BindVal n':) $ bindArgs xs ty
          return ((Arg n' t':ns),ty')
    (args',ty') <- bindArgs args ty
    return $ DefRef b' args' ty'
  Raw.TypeRef ta b z refines -> do
    let ta' = convertTA ta
    b' <- newBinding b
    z' <- newBinding z
    let tType = BindType b'
    let zVar = BindVal z'
    refines' <- local ([zVar,tType] ++) $ mapM bindRefine refines
    return $ TypeRef ta' b' z' refines'
  Raw.MemberRef ta b bound ty -> do
    let ta' = convertTA ta
    b' <- newBinding b
    let bound' = case bound of
                  Raw.LEQ -> LEQ
                  Raw.EQQ -> EQQ
                  Raw.GEQ -> GEQ
    ty' <- bindType ty
    return $ MemberRef ta' b' bound' ty'
  Raw.SubtypeRef t1 t2 -> do
    t1' <- bindType t1
    t2' <- bindBaseType t2
    return $ SubtypeRef t1' t2'
    
bindExpr :: Raw.Expr -> BindMonad Expr
bindExpr e = case e of
  Raw.PathExpr p -> do
    p' <- bindPath p
    return $ PathExpr p'
  Raw.New ty name decls -> do
    ty'    <- bindType ty
    b      <- newBinding name
    decls' <- local ((BindVal b):) $ mapM bindDecl decls
    return $ New ty' b decls'
  Raw.Call path args -> do
    path' <- bindCallPath path
    args' <- mapM bindPath args
    return $ Call path' args'
  Raw.IntLit i -> return $ IntLit i
  Raw.UnitLit -> return UnitLit  
  Raw.UndefLit -> return UndefLit

--hmm lots of code duplication here
bindPath :: Raw.Path -> BindMonad Path
bindPath p = case p of
  Raw.Var v -> do
    b <- fetchVal v
    return $ Var b
  Raw.Field path name -> do
    path' <- bindPath path
    return $ Field path' name

bindCallPath :: Raw.Path -> BindMonad Path
bindCallPath p = case p of
  Raw.Var v -> do
    b <- fetchDef v
    return $ Var b
  Raw.Field path name -> do
    path' <- bindPath path
    return $ Field path' name

bindBaseType :: Raw.BaseType -> BindMonad BaseType
bindBaseType b =  
  case b of
    Raw.UnitType   -> return UnitType
    Raw.BotType    -> return BotType
    Raw.PathType p ->
      case p of
        Raw.Var v -> do
          v' <- fetchType v
          return $ PathType $ Var v'
        Raw.Field _ _ -> do
          p' <- bindPath p
          return $ PathType p'

bindType :: Raw.Type -> BindMonad Type
bindType (Raw.Type b rs) = do
  b' <- bindBaseType b
  rs' <- mapM bindRefine rs
  return $ Type b' rs'
