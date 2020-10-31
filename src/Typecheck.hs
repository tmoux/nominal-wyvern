module Typecheck where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List (find)
import Syntax
import TypeUtil
import PrettyPrint
import Text.Printf
import Debug.Trace

data Error = OtherErr String
    deriving (Show)

type Context = [Refinement]
type TCMonad = ReaderT Context (Except String)

assert :: String -> Bool -> TCMonad ()
assert err True = return ()
assert err False = throwError err

lookupCtx :: (Refinement -> Bool) -> String -> TCMonad Refinement
lookupCtx pred msg = do
  lookup <- reader $ find pred
  case lookup of
    Just x -> return x
    Nothing -> throwError msg

searchCtx :: (Refinement -> TCMonad Bool) -> TCMonad Bool
searchCtx pred = do
    gamma <- ask
    f gamma 
    where f [] = return False
          f (x:xs) = pred x ||^ f xs

typecheck prog = runReaderT (typecheckProgram prog) []

typecheckProgram :: Program -> TCMonad Type
typecheckProgram (Program decls expr) = do
    refines <- getDecls decls    
    local (refines ++) $ typecheckExpr expr
    where getDecls []     = return []
          getDecls (d:ds) = do
            r  <- typecheckDecl d
            rs <- local (r:) $ getDecls ds
            return (r:rs)

typecheckDecl :: Declaration -> TCMonad Refinement
typecheckDecl d = case d of 
    --ValDecl b Nothing e -> do
    --    ty <- typecheckExpr e
    --    return $ ValRef b ty
    ValDecl b ty e -> do
        exprTy <- typecheckExpr e
        isSubtype exprTy ty >>= assert (printf "val %s: %s is not a subtype of %s" (show b) (show exprTy) (show ty))
        return $ ValRef b ty
    DefDecl method args retTy prog -> do
        let argTypes = map (\(Arg _ t) -> t) args
        let argVals = map (\(Arg b t) -> ValRef b t) args
        (local (argVals++) $ (checkAll typeWF argTypes)) >>= assert (printf "%s: argument types not wf" (show method))
        progTy <- local (argVals++) $ typecheckProgram prog
        (local (argVals++) $ isSubtype progTy retTy) >>= assert (printf "def %s: invalid subtype: expected %s, got %s)" (show method) (show retTy) (show progTy))
        return $ DefRef method args retTy        
    TypeDecl ta t z decls -> do
        let zt = ValRef z (makeNomType t)
        let tt = TypeRef ta t z decls
        (local ([tt,zt]++) $ checkAll refineWF decls) >>= assert (printf "type %s not wf" (show t))
        return tt
    TypeEqDecl b ty -> do
        typeWF ty >>= assert (printf "type %s not wf" (show ty))
        return $ MemberRef Material b EQQ ty -- material?
    SubtypeDecl t1 t2 -> do
        refineWF (SubtypeRef t1 t2) >>= assert (printf "invalid subtype decl: %s %s" (show t1) (show t2))
        return $ SubtypeRef t1 t2

typecheckExpr :: Expr -> TCMonad Type
typecheckExpr e = case e of
    PathExpr p -> typecheckPath p
    New ty z rs -> do
        (z_old,rs_old) <- unfold ty
        let tcDecl d = case d of
                         --ValDecl _ _ _ -> typecheckDecl d
                         _             -> local (ValRef z ty:) $ typecheckDecl d
        rs' <- mapM tcDecl rs
        let rs_old' = map (substRefines (Var z) z_old) rs_old
        --assert that new expr is structural subtype of ty
        local (ValRef z (merge ty (ref rs')):) $ checkPerm isSubtypeRef rs' rs_old' 
          >>= assert ("invalid new expression:" ++ (show ty))
        return ty
    Call p es -> do
        (methodName,ctxOp) <- case p of 
          Var (Binding b _) -> return (b,id) --top-level function, use regular context
          Field path name -> do
            pty <- typecheckPath path
            (z,decls) <- unfold pty
            return (name, const $ map (substRefines path z) decls)
        let pred (DefRef (Binding b _) _ _) = b == methodName
            pred _ = False
        DefRef m args retTy <- local ctxOp $ lookupCtx pred ("failed to find method name " ++ methodName)

        --subfunc creates the correct type by subbing in the arguments
        let subfunc ty = foldr (\(Arg x _,exp) -> substType exp x) ty (zip args es)
        let argsTypeSubbed = map subfunc (map (\(Arg _ ty) -> ty) args) --correct arg types
        esTys <- mapM typecheckPath es --these are the calling types
        --subtype check
        (return (length esTys == length argsTypeSubbed))
          >>= assert (printf "calling %s: wrong # of arguments" methodName)
        checkPairwise isSubtype esTys argsTypeSubbed 
          >>= assert (printf "calling %s: subtype check failed when calling method" methodName)
        return $ subfunc retTy --subbed return type
    IntLit _ -> do
        TypeRef _ b _ _ <- lookupCtx pred "failed to find type Int"
        return $ makeNomType b
        where pred (TypeRef _ (Binding b' _) _ _) = b' == "Int"
              pred _ = False
    UnitLit -> return theUnit

typecheckPath :: Path -> TCMonad Type
typecheckPath p = case p of
    Var b -> do
        ValRef _ ty <- lookupCtx pred ("failed to find var " ++ show b)
        return ty
        where pred (ValRef b' _) = b == b'
              pred _ = False
    Field p n -> do
        tau <- typecheckPath p
        (z,decls) <- unfold tau
        ValRef _ tauv <- local (const decls) $ lookupCtx pred ("failed to find field " ++ n)
        return $ substType p z tauv
        where pred (ValRef (Binding b _) _) = b == n
              pred _ = False

--unfold
unfoldBaseType :: BaseType -> TCMonad (Binding,[Refinement])
unfoldBaseType base = case base of
    UnitType -> return (Binding "z" (-1),[]) --unit binding
    PathType p -> case p of
      Var x -> do
        TypeRef _ _ z rs <- lookupCtx pred ("unfold: failed to find type " ++ show x)
        return (z,rs)
        where pred (TypeRef _ b _ _) = b == x
              pred _ = False
      Field pa na -> do
        tau <- typecheckPath pa
        (z,decls) <- unfold tau
        d <- local (const decls) $ lookupCtx pred ("unfold: failed to find field " ++ na)
        case d of
          TypeRef _ _ z' rs' -> return (z',map (substRefines pa z) rs')
          MemberRef _ _ bound ty -> case bound of
            GEQ -> return (Binding "z" (-1),[]) --unit binding
            _ -> unfold $ substType pa z ty
          where pred (TypeRef _ (Binding b _)  _ _) = b == na
                pred (MemberRef _ (Binding b _) _ _) = b == na 
                pred _ = False
    _  -> throwError (printf "couldn't unfold %s: shouldn't happen?" (show base))

unfold :: Type -> TCMonad (Binding,[Refinement])
unfold (Type base rs) = do
  (z,baseRs) <- unfoldBaseType base  
  return (z,mergeRefs rs baseRs)

--type equality
equalBaseType :: BaseType -> BaseType -> TCMonad Bool
equalBaseType a b = 
  {-trace ("eq base type " ++ show a ++ " " ++ show b) $-} case (a,b) of
    (UnitType,UnitType) -> return True
    (BotType,BotType)   -> return True
    (PathType (Var n1), PathType (Var n2)) -> return $ n1 == n2
    (PathType (Field p1 n1), PathType (Field p2 n2)) -> do
      if p1 == p2 then return $ n1 == n2
        else do
          tau1 <- typecheckPath p1
          tau2 <- typecheckPath p2
          eqTy <- equalType tau1 tau2
          return $ eqTy && (n1 == n2)
    _ -> return False  

equalType :: Type -> Type -> TCMonad Bool
equalType (Type b1 r1) (Type b2 r2) =
  equalBaseType b1 b2 &&^ checkPermDual equalRef r1 r2
  --traceM $ (show (Type b1 r1)) ++ " = " ++ (show (Type b2 r2))

equalRef :: Refinement -> Refinement -> TCMonad Bool
equalRef r1 r2 =
  case (r1,r2) of
    (ValRef b1 t1,ValRef b2 t2) -> do
      (return $ b1 == b2) &&^ equalType t1 t2
    (TypeRef _ b@(Binding t1 _) z1 r1,TypeRef _ (Binding t2 _) z2 r2) -> do
      let r2' = map (substRefines (Var z1) z2) r2  
      eqRefs <- local (ValRef z1 (makeNomType b):) $ checkPermDual equalRef r1 r2'
      return $ t1 == t2 && eqRefs
    (DefRef (Binding b1 _) args1 t1,DefRef (Binding b2 _) args2 t2) -> do
      let types1 = map (\(Arg _ t) -> t) args1
      let types2 = map (\(Arg _ t) -> t) args2
      eqTypes <- checkPairwise equalType types1 types2
      return $ b1 == b2 && length args1 == length args2 && eqTypes
    (SubtypeRef s1 t1,SubtypeRef s2 t2) -> do
      equalType s1 s2 &&^ equalBaseType t1 t2
    (MemberRef _ (Binding b1 _) bound1 t1,MemberRef _ (Binding b2 _) bound2 t2) -> do
      eqTy <- equalType t1 t2
      return $ b1 == b2 && bound1 == bound2 && eqTy
    _ -> return False    

--subtyping 
isSubtype :: Type -> Type -> TCMonad Bool
isSubtype t1@(Type b1 r1) t2@(Type b2 r2) =  do
  --traceM (show t1 ++ " <: " ++ show t2)
  eqBase <- equalBaseType b1 b2 
  if eqBase then checkPerm isSubtypeRef r1 r2 
            else recLHS ||^ recRHS ||^ normalAns
  where 
    pred name (MemberRef _ (Binding b _) _ _) = name == b
    pred _ _ = False
    recLHS = case b1 of
      PathType (Field path name) -> do
        pathTy <- typecheckPath path
        (z,prs) <- unfold pathTy
        MemberRef _ _ bound ty <- local (const prs) $ lookupCtx (pred name) ("failed to find type member " ++ name)
        case bound of
          GEQ -> return False
          _   -> do
            let subbedType = substType path z $ merge ty r1
            isSubtype subbedType t2
      _ -> return False
    recRHS = case b2 of
      PathType (Field path name) -> do
        pathTy@(Type base rs) <- typecheckPath path
        (z,prs) <- unfold pathTy
        MemberRef _ _ bound ty <- local (const prs) $ lookupCtx (pred name) ("failed to find type member " ++ name)
        case bound of
          LEQ -> return False
          _   -> do
            let subbedType = substType path z $ merge ty r2
            isSubtype t1 subbedType
      _ -> return False
    normalAns = case b1 of
      BotType -> return True
      _ ->  equalType t2 theUnit 
        ||^ (isSubtypeBase t1 b2 &&^ do
              (z,r1') <- unfold t1
              local (ValRef z t1:) $ checkPerm isSubtypeRef r1' r2)
 
isSubtypeBase :: Type -> BaseType -> TCMonad Bool
isSubtypeBase (Type b1 r1) b2 = do
  --traceM ("isb: " ++ show (Type b1 r1) ++ " <: " ++ show b2)
  equalBaseType b1 b2 ||^ do
    ctxOp <- case b1 of
      PathType (Field path name) -> do
        pathTy <- typecheckPath path
        (z,prs) <- unfold pathTy
        return $ const $ map (substRefines path z) prs
      _ -> return id
    local ctxOp $ searchCtx pred
  where pred (SubtypeRef (Type baseL rsL) baseR) =
             equalBaseType b1 baseL 
         &&^ checkPerm isSubtypeRef r1 rsL 
         &&^ isSubtypeBase (Type baseR r1) b2
        pred _ = return False

isSubtypeRef :: Refinement -> Refinement -> TCMonad Bool
isSubtypeRef a b = {-trace (show a ++ " <: " ++ show b) $-} case (a,b) of
  (ValRef (Binding b1 _) t1,ValRef (Binding b2 _) t2) -> do
    (return $ b1 == b2) &&^ isSubtype t1 t2
  (DefRef (Binding b1 _) args1 t1,DefRef (Binding b2 _) args2 t2) -> do
    let types1 = map (\(Arg _ t) -> t) args1
    let types2 = map (\(Arg _ t) -> t) args2
    let vs1 = map (\(Arg v t) -> ValRef v t) args1
    let vs2 = map (\(Arg v t) -> ValRef v t) args2
    argsSubtype <- local ((vs1 ++ vs2) ++) $ checkPerm isSubtype types2 types1 --contra
    retSubtype <- local ((vs1 ++ vs2) ++) $ isSubtype t1 t2 --cov
    return $ (b1 == b2) && argsSubtype && retSubtype
  (TypeRef _ b@(Binding t1 _) z1 rs1,TypeRef _ (Binding t2 _) z2 rs2) -> do
    let rs2' = map (substRefines (Var z1) z2) rs2
    isEq <- local (ValRef z1 (makeNomType b):) $ checkPerm equalRef rs1 rs2'
    return $ (t1 == t2) && isEq
  (MemberRef _ (Binding b1 _) bound1 t1,MemberRef _ (Binding b2 _) bound2 t2) -> do
    if b1 == b2 then
      case (bound1,bound2) of
        (EQQ,EQQ) -> checkCov &&^ checkContra --checkEq?
        (LEQ,LEQ) -> checkCov
        (EQQ,LEQ) -> checkCov
        (GEQ,GEQ) -> checkContra
        (EQQ,GEQ) -> checkContra
        _         -> return False
      else return False
        where checkEq      = equalType t1 t2
              checkCov     = isSubtype t1 t2
              checkContra  = isSubtype t2 t1
  (SubtypeRef s1 t1,SubtypeRef s2 t2) -> do
    equalType s1 s2 &&^ equalBaseType t1 t2
  _ -> return False
--WF checks
typeWF :: Type -> TCMonad Bool
typeWF (Type _ []) = return True
typeWF t@(Type base rs) = do
  (z,rs1) <- unfold t
  (_,rs2) <- unfoldBaseType base
  local (ValRef z t:) $ checkPerm isSubtypeRef rs1 rs2

refineWF :: Refinement -> TCMonad Bool
refineWF r = case r of
  ValRef _ t -> typeWF t
  DefRef _ args retTy -> do
    let vs = map (\(Arg b t) -> ValRef b t) args
    argsWF <- local (vs++) $ checkAll refineWF vs
    retTyWF <- local (vs++) $ typeWF retTy
    return $ argsWF && retTyWF
  tr@(TypeRef _ t z rs) -> do
    local ([ValRef z (makeNomType t),tr]++) $ checkAll refineWF rs
  MemberRef _ _ _ t -> typeWF t
  s@(SubtypeRef t1 t2) -> do
    (z,rs)   <- unfold t1
    (z',rs') <- unfoldBaseType t2
    local ([ValRef z t1,s]++) $ checkPerm isSubtypeRef rs (map (substRefines (Var z) z') rs')
