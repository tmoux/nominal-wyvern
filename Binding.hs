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

type BindMonad = ExceptT String (ReaderT [BindCtx] (State Int))

newBinding :: Name -> BindMonad Binding
newBinding name = do
    cnt <- get
    put (cnt+1)
    return $ Binding name cnt

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

toBindCtx :: Declaration -> Maybe BindCtx
toBindCtx d = case d of
  ValDecl b _    -> Just $ BindVal b
  TypeDecl b _ _ -> Just $ BindType b
  _ -> Nothing

bind prog = evalState (
              runReaderT (
                runExceptT (bindProgram prog)
              ) []
            ) 0

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
  Raw.ValDecl b e -> do  
    b' <- newBinding b   
    e' <- bindExpr e
    return $ ValDecl b' e'
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
  Raw.TypeDecl b z refines -> do
    b' <- newBinding b
    z' <- newBinding z
    let tType = BindType b'
    let zVar = BindVal z'    
    refines' <- local ([zVar,tType] ++) $ mapM bindRefine refines
    return $ TypeDecl b' z' refines'
  Raw.TypeEqDecl b ty -> do
    b' <- newBinding b
    ty' <- bindType ty
    return $ TypeEqDecl b' ty'
  Raw.SubtypeDecl t1 t2 -> do
    t1' <- bindType t1
    t2' <- bindType t2
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
  Raw.TypeRef b z refines -> do
    b' <- newBinding b
    z' <- newBinding z
    let tType = BindType b'
    let zVar = BindVal z'
    refines' <- local ([zVar,tType] ++) $ mapM bindRefine refines
    return $ TypeRef b' z' refines'
  Raw.MemberRef b bound ty -> do
    b' <- newBinding b
    let bound' = case bound of
                  Raw.LEQ -> LEQ
                  Raw.EQQ -> EQQ
                  Raw.GEQ -> GEQ
    ty' <- bindType ty
    return $ MemberRef b' bound' ty'
  Raw.SubtypeRef t1 t2 -> do
    t1' <- bindType t1
    t2' <- bindType t2
    return $ SubtypeRef t1' t2'
    
bindExpr :: Raw.Expr -> BindMonad Expr
bindExpr e = case e of
  Raw.PathExpr p -> do
    p' <- bindPath p
    return $ PathExpr p'
  Raw.New name ty decls -> do
    b      <- newBinding name
    ty'    <- bindType ty
    decls' <- mapM bindDecl decls
    return $ New b ty' decls'
  Raw.Call path args -> do
    path' <- bindPath path
    args' <- mapM bindPath args
    return $ Call path' args'
  Raw.IntLit i -> return $ IntLit i
  Raw.UnitLit -> return UnitLit  

bindPath :: Raw.Path -> BindMonad Path
bindPath p = case p of
  Raw.Var v -> do
    b <- fetchVal v
    return $ Var b
  Raw.Field path name -> do
    path' <- bindPath path
    return $ Field path' name

bindType :: Raw.Type -> BindMonad Type
bindType (Raw.Type b rs) = do
  b' <- 
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
  rs' <- mapM bindRefine rs
  return $ Type b' rs'
