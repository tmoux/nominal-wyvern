module Typecheck where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Extra
import Data.List (find, partition)
import Syntax
import TypeUtil
import PrettyPrint
import Text.Printf
import Data.Functor.Identity
import Debug.Trace

instance MonadFail Data.Functor.Identity.Identity where
  fail = error "monad pattern match fail"

data Context = Context
  { toplevel :: [TopLevelDeclaration]
  , gamma    :: [MemberDeclaration]
  }
emptyCtx = Context [] []
appendTopLevel :: [TopLevelDeclaration] -> Context -> Context
appendTopLevel ds (Context t g) = Context (ds++t) g
appendGamma :: [MemberDeclaration] -> Context -> Context
appendGamma ds (Context t g) = Context t (ds++g)
type TCMonad = ReaderT Context (Except String)

assert :: String -> Bool -> TCMonad ()
assert _   True  = return ()
assert err False = throwError err

lookupMemberDecls :: (MemberDeclaration -> Bool) -> String -> [MemberDeclaration] -> TCMonad MemberDeclaration
lookupMemberDecls pred msg list =
  case find pred list of
    Just x  -> return x
    Nothing -> throwError msg

lookupTLDecls :: (TopLevelDeclaration -> Bool) -> String -> TCMonad TopLevelDeclaration
lookupTLDecls pred msg = do
  lookup <- reader (find pred . toplevel)
  case lookup of
    Just x  -> return x
    Nothing -> throwError msg

searchCtx :: (MemberDeclaration -> TCMonad Bool) -> [MemberDeclaration] -> TCMonad Bool
searchCtx pred list = m_or =<< mapM pred list

typecheck prog = runExcept (runReaderT (typecheckProgram prog) emptyCtx)

typecheckProgram :: Program -> TCMonad Type
typecheckProgram (Program decls expr) = do
    let (names,subs) = partition split decls
    mapM_ checkTLDecl names
    local (appendTopLevel decls) $ do
      mapM_ checkTLDecl subs
      typecheckExpr expr
    where split (NameDecl _ _ _ _) = True
          split (SubtypeDecl _ _)  = False

checkTLDecl :: TopLevelDeclaration -> TCMonad ()
checkTLDecl (NameDecl _ _ z decls) = do
  let fields = getFields decls
      paths = map (\v -> Field (Var z) v) fields
      types = getTypes decls
  mapM_ (noReference paths) types
  where getFields [] = []
        getFields ((ValDecl v _):xs) = name v:getFields xs
        getFields (_:xs) = getFields xs
        getTypes [] = []
        getTypes (TypeMemDecl _ _ _ ty:xs) = ty:getTypes xs
        getTypes (_:xs) = getTypes xs
        noReference paths ty = mapM_ (notInType ty) paths
        notInType (Type base rs) path = do
          case base of
            PathType p t -> notInPath p path 
            _ -> return ()
          mapM_ (notInRefine path) rs
        notInRefine path (RefineDecl _ _ ty) = notInType ty path
        notInPath (Var _) path = return ()
        notInPath (Field p n) path
          | p == path = throwError (printf "error when checking name well-formedness: sibling field %s found in path %s" (show path) (show p))
          | otherwise = notInPath p path 
checkTLDecl (SubtypeDecl t1 n2) = do
  (x1,decls1) <- unfoldExpanded t1
  (x2,decls2) <- unfoldExpanded (Type n2 [])
  local (appendGamma [ValDecl x1 t1]) $
    isStructSubtype decls1 (subst (Var x1) x2 decls2) >>= msg
  where msg = assert (printf "invalid subtype decl: %s not a subtype of %s" (show t1) (show n2))

typeNB :: Type -> TCMonad ()
typeNB (Type base rs) = case base of
  TopType -> return ()
  NamedType n -> return ()
  BotType -> throwError "bot found when doing NB check"
  PathType p t -> do
    tau_p <- typecheckPath p
    (z,decls) <- unfoldExpanded =<< typeExpand tau_p
    TypeMemDecl _ _ bound ty <- lookupMemberDecls pred errMsg decls
    case bound of
      EQQ -> typeNB ty
      _   -> throwError (printf "type member %s does not have an exact bound when doing NB check" (show t))
    where pred (TypeMemDecl _ t' _ _) = t == name t'
          pred _ = False 
          errMsg = printf "typeNB: couldn't find type member %s in path %s" t (show p)

defnWF :: MemberDefinition -> TCMonad ()
defnWF d = case d of
  TypeMemDefn _ _ -> return ()
  ValDefn v tau_v expr -> do
    tau_v' <- typecheckExpr expr
    isSubtype tau_v' tau_v >>= assert (printf "val %s: %s is not a subtype of annotation %s" (show v) (show tau_v') (show tau_v))
  DefDefn f args tau_r expr -> do
    let args' = map argToDecl args
    local (appendGamma args') $ do
      tau_r' <- typecheckExpr expr
      isSubtype tau_r' tau_r >>= assert (printf "defn %s: %s is not a subtype of return type %s" (show f) (show tau_r') (show tau_r))

newTypeWF :: Type -> Binding -> [MemberDefinition] -> TCMonad ()
newTypeWF ty z defns = do
  typeNB ty
  Type n rs <- typeExpand ty
  (x_n,decls_n) <- unfoldExpanded (Type n rs)
  let tau_x = Type n (ref.sig $ defns)
      self = ValDecl x_n tau_x
  local (appendGamma [self]) $ isStructSubtype (sig defns) (decls_n ++ map refToDecl rs)
  let checkDefn d@(DefDefn _ _ _ _) = local (appendGamma [self]) $ defnWF d
      checkDefn d = defnWF d
  mapM_ checkDefn defns

unfold :: Type -> TCMonad (Binding,[MemberDeclaration])
unfold = typeExpand >=> unfoldExpanded

unfoldExpanded :: Type -> TCMonad (Binding,[MemberDeclaration])
unfoldExpanded (Type base rs) =
  case base of
    NamedType n -> do
      NameDecl _ _ z decls <- lookupTLDecls pred msg 
      return (z,decls ++ map refToDecl rs)
      where pred (NameDecl _ n' _ _) = n == n'
            pred _ = False
            msg = printf "couldn't find name %s" (show n)
    _ -> return (Binding "z" (-1),map refToDecl rs)

typeExpand :: Type -> TCMonad Type
typeExpand tau@(Type base rs) = case base of
  PathType p t -> do
    tau_p <- typecheckPath p
    (z,decls) <- unfold tau_p
    TypeMemDecl _ _ bound ty <- lookupMemberDecls pred errMsg decls
    case bound of
      GEQ -> return tau
      _   -> typeExpand (subst p z (merge ty rs))
    where pred (TypeMemDecl _ t' _ _) = t == name t'
          pred _ = False 
          errMsg = printf "type expand: couldn't find type member %s in path %s" t (show p)
  _ -> return tau  

typecheckExpr :: Expr -> TCMonad Type
typecheckExpr e = case e of
    PathExpr p -> typecheckPath p
    New ty z rs -> do
      newTypeWF ty z rs
      return ty
    Call p meth es -> do
      pty <- typecheckPath p
      (z,decls) <- unfold pty
      let pred (DefDecl b _ _) = name b == meth
          pred _ = False
      DefDecl m args retTy <- lookupMemberDecls pred cantFindDef decls
      --subfunc creates the correct type by subbing in the arguments and path p
      let subfunc ty = foldr (\(Arg x _,exp) -> subst exp x) (subst p z ty) (zip args es)
      let argsTypeSubbed = map subfunc (map (\(Arg _ ty) -> ty) args) --correct arg types
      esTys <- mapM typecheckPath es --these are the calling types
      --subtype check
      (return (length esTys == length argsTypeSubbed)) >>= assert wrongLength
      checkPairwise isSubtype esTys argsTypeSubbed >>= assert (callNotSubtype esTys argsTypeSubbed)
      return $ subfunc retTy --subbed return type
        where cantFindDef    = "failed to find method name " ++ meth
              wrongLength    = printf "calling %s: wrong # of arguments" meth
              callNotSubtype esTys argsTypeSubbed = (printf "calling %s: subtype check failed when calling method, %s not subtypes of %s" meth (show esTys) (show argsTypeSubbed))
    Let x e1 e2 -> do
      t1 <- typecheckExpr e1
      local (appendGamma [ValDecl x t1]) $ typecheckExpr e2
    TopLit   -> return theTop
    UndefLit -> return theBot

typecheckPath :: Path -> TCMonad Type
typecheckPath p = case p of
    Var b -> do
        ctx <- ask
        ValDecl _ ty <- lookupMemberDecls pred errMsg (gamma ctx)
        return ty
        where pred (ValDecl b' _) = b == b'
              pred _ = False
              errMsg = ("failed to find var " ++ show b)
    Field p v -> do
        tau <- typecheckPath p
        (z,decls) <- unfold tau
        ValDecl _ tauv <- lookupMemberDecls pred errMsg decls
        return $ subst p z tauv
        where pred (ValDecl b _) = name b == v
              pred _ = False
              errMsg = ("failed to find field " ++ v)

--type equality
equalBaseType :: BaseType -> BaseType -> TCMonad Bool
equalBaseType a b =
  {-trace ("eq base type " ++ show a ++ " " ++ show b) $-} case (a,b) of
    (TopType,TopType) -> return True
    (BotType,BotType) -> return True
    (NamedType n1,NamedType n2) -> return $ n1 == n2
    (PathType p1 n1, PathType p2 n2)
      | p1 == p2 -> return $ n1 == n2
      | otherwise -> do
          tau1 <- typecheckPath p1
          tau2 <- typecheckPath p2
          eqTy <- undefined --equalType tau1 tau2
          return $ eqTy && (n1 == n2)
    _ -> return False

equalType :: Type -> Type -> TCMonad Bool
equalType (Type b1 r1) (Type b2 r2) =
  equalBaseType b1 b2 &&^ checkPermDual equalRefinement r1 r2
  --traceM $ (show (Type b1 r1)) ++ " = " ++ (show (Type b2 r2))

equalRefinement :: Refinement -> Refinement -> TCMonad Bool
equalRefinement (RefineDecl t1 bound1 ty1) (RefineDecl t2 bound2 ty2)
  = (return $ name t1 == name t2 && bound1 == bound2) &&^ equalType ty1 ty2

--subtyping
--TODO: re-implement this and the isSubtypeBase function
isSubtype :: Type -> Type -> TCMonad Bool
isSubtype t1@(Type b1 r1) t2@(Type b2 r2) =  do
  --traceM (show t1 ++ " <: " ++ show t2)
  eqBase <- equalBaseType b1 b2
  if eqBase then checkPerm isSubtypeRef r1 r2
            else normalAns ||^ recLHS ||^ recRHS
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
      _ -> equalType t2 theUnit
       ||^ (isSubtypeBase t1 b2 &&^ do
           checkPerm isSubtypeRef r1 r2)
           --this (above) is the old rule, the next two lines is the new rule
           --(z,r1') <- unfold t1
           --local (ValRef z t1:) $ checkPerm isSubtypeRef r1' r2)
{-
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
-}

isSubtypeRef :: Refinement -> Refinement -> TCMonad Bool
isSubtypeRef a b = isSubtypeMemDecl (refToDecl a) (refToDecl b)

isStructSubtype :: [MemberDeclaration] -> [MemberDeclaration] -> TCMonad Bool
isStructSubtype as bs = checkPerm isSubtypeMemDecl as bs

isSubtypeMemDecl :: MemberDeclaration -> MemberDeclaration -> TCMonad Bool
isSubtypeMemDecl a b = {-trace (show a ++ " <: " ++ show b) $-} case (a,b) of
  (TypeMemDecl _ t1 bound1 ty1,TypeMemDecl _ t2 bound2 ty2)
    | name t1 == name t2 -> case (bound1,bound2) of
        (EQQ,EQQ) -> checkCov &&^ checkContra --checkEq?
        (LEQ,LEQ) -> checkCov
        (EQQ,LEQ) -> checkCov
        (GEQ,GEQ) -> checkContra
        (EQQ,GEQ) -> checkContra
        _         -> return False
    | otherwise -> return False
    where checkCov     = isSubtype ty1 ty2
          checkContra  = isSubtype ty2 ty1
  (ValDecl v1 t1,ValDecl v2 t2) ->
    (return $ name v1 == name v2) &&^ isSubtype t1 t2
  (DefDecl f1 args1 ty1,DefDecl f2 args2 ty2) -> do
    let types1 = map argType args1
        types2 = map argType args2
        names1 = map argName args1
        names2 = map argName args2
        subfunc ty = foldr (\(x1,x2) -> subst (Var x1) x2) ty (zip names1 names2)
        vs1 = map argToDecl args1
        types2' = map subfunc types2
        ty2' = subfunc ty2
    local (appendGamma vs1) $ do
      argsSubtype <- checkPerm isSubtype types2' types1 --contra
      retSubtype <- isSubtype ty1 ty2' --cov
      return $ (name f1 == name f2) && argsSubtype && retSubtype
  _ -> return False
