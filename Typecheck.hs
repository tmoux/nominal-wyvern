module Typecheck where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List (find)
import Syntax
import PrettyPrint
import Text.Printf

data Error = OtherErr String
    deriving (Show)

type Context = [Refinement]
type TCMonad = ExceptT String (Reader Context)

lookupCtx :: (Refinement -> Bool) -> TCMonad Refinement
lookupCtx pred = do
    gamma <- ask
    case (find pred gamma) of
        Just x -> return x
        Nothing -> throwError $ "lookup failed"

typecheck prog = runReader (
                   runExceptT (typecheckProgram prog)
                 ) []

typecheckProgram :: Program -> TCMonad Type
typecheckProgram (Program decls expr) = do
    refines <- getDecls decls    
    local (refines ++ ) $ typecheckExpr expr
    where getDecls []     = return []
          getDecls (d:ds) = do
            r <- typecheckDecl d
            rs <- local (\ctx -> r:ctx) $ getDecls ds
            return (r:rs)

typecheckDecl :: Declaration -> TCMonad Refinement
typecheckDecl d = case d of 
    ValDecl b e -> do
        ty <- typecheckExpr e
        return $ ValRef b ty
    DefDecl method args retTy prog -> do
        let argTypes = map (\(Arg _ t) -> t) args
        (checkAll typeWF argTypes) >>= assert "argument types not wf"
        let argVals = map (\(Arg b t) -> ValRef b t) args
        progTy <- local (argVals++) $ typecheckProgram prog
        (local (argVals++) $ isSubtype progTy retTy) >>= assert "def expression has invalid subtype"
        return $ DefRef method args retTy        
    TypeDecl t z decls -> do
        let zt = ValRef z (makeNomType t)
        let tt = TypeRef t z decls
        (local ([tt,zt]++) $ checkAll refineWF decls) >>=
          assert (printf "type %s not wf" (show t))
        return tt
    TypeEqDecl b ty -> do
        typeWF ty >>= assert ("type " ++ (show ty) ++ " not wf")
        return $ MemberRef b EQQ ty
    SubtypeDecl t1 t2 -> do
        subtypeWF t1 t2 >>= assert (printf "invalid subtype decl: %s %s" (show t1) (show t2))
        return $ SubtypeRef t1 t2

typecheckExpr :: Expr -> TCMonad Type
typecheckExpr e = case e of
    PathExpr p -> typecheckPath p
    New ty z decls -> do
        (z',rs) <- unfold ty
        let tcDecl d = case d of
                         ValDecl _ _ -> typecheckDecl d
                         _           -> local (ValRef z ty:) $ typecheckDecl d
        refines <- mapM tcDecl decls
        let refines' = map (substRefines z (Var z')) refines
        --assert that new expr is structural subtype of ty
        checkPerm isSubtypeRef refines' rs >>= assert "invalid new expression"
        return ty
    Call p es -> do
        (methodName,ctx) <- case p of 
          Var (Binding b _) -> return (b,id) --top-level function, use regular context
          Field path name -> do
            pty <- typecheckPath path
            (z,decls) <- unfold pty
            return (name, const $ map (substRefines z path) decls)
        let pred (DefRef (Binding b _) _ _) = b == methodName
            pred _ = False
        DefRef m args retTy <- local ctx $ lookupCtx pred

        --subfunc creates the correct type by subbing in the arguments
        let subfunc ty = foldr (\(Arg x _,exp) -> substType x exp) ty (zip args es)
        let argsTypeSubbed = map subfunc (map (\(Arg _ ty) -> ty) args) --correct arg types
        esTys <- mapM typecheckPath es --calling types
        --subtype check
        (return (length esTys == length argsTypeSubbed))
          >>= assert "wrong # of arguments"
        checkPairwise isSubtype esTys argsTypeSubbed 
          >>= assert "Subtype check failed when calling method"
        let subTy = subfunc retTy --correct return type
        return subTy
    IntLit _ -> do
        TypeRef b _ _ <- lookupCtx pred
        return $ makeNomType b
        where pred (TypeRef (Binding b' _) _ _) = b' == "Int"
              pred _ = False
    UnitLit -> return theUnit

typecheckPath :: Path -> TCMonad Type
typecheckPath p = case p of
    Var b -> do
        ValRef _ ty <- lookupCtx pred
        return ty
        where pred (ValRef b' _) = b == b'
              pred _ = False
    Field p n -> do
        tau <- typecheckPath p
        (z,decls) <- unfold tau
        ValRef _ tauv <- local (const decls) $ lookupCtx pred
        return $ substType z p tauv
        where pred (ValRef (Binding b _) _) = b == n
              pred _ = False

unfold :: Type -> TCMonad (Binding,[Refinement])
unfold (Type base rs) = do
  (z,baseRs) <- case base of
    UnitType -> return (Binding "z" (-1),[]) --unit binding
    PathType p -> case p of
      Var x -> do
        TypeRef _ z rs <- lookupCtx pred
        return (z,rs)
        where pred (TypeRef b _ _) = b == x
              pred _ = False
      Field pa na -> do
        tau <- typecheckPath pa
        (z,decls) <- unfold tau
        d <- local (const decls) $ lookupCtx pred
        case d of
          TypeRef _ z' rs' -> return (z',map (substRefines z pa) rs')
          MemberRef _ bound ty -> case bound of
            GEQ -> return (Binding "z" (-1),[]) --unit binding
            _ -> unfold $ substType z pa ty
          where pred (TypeRef (Binding b _)  _ _) = b == na
                pred (MemberRef (Binding b _) _ _) = b == na 
                pred _ = False
    _  -> throwError "unfold: shouldn't happen?"
  return (z,rs ++ baseRs) --maybe merge/overwrite the old decls?

assert :: String -> Bool -> TCMonad ()
assert err True = return ()
assert err False = throwError err

--check that all elements satisfy predicate
checkAll :: (a -> TCMonad Bool) -> [a] -> TCMonad Bool
checkAll f as = foldM (\res a -> if res then f a else return False) True as
--check that f is true for all zipped pairs
checkPairwise :: (a -> a -> TCMonad Bool) -> [a] -> [a] -> TCMonad Bool
checkPairwise f as bs = foldM (\res (a,b) -> if res then f a b else return False) True (zip as bs)

--check that for all b in bs, there exists an a s.t. (f a b) is true
--yikes is there a better way to do this? monads are hard
checkPerm :: (a -> a -> TCMonad Bool) -> [a] -> [a] -> TCMonad Bool
checkPerm f as bs = 
  foldM (\res b -> if res then search b else return False) True bs
  where search b = g b as
        g b [] = return False
        g b (a:as) = do
          res <- f a b
          if res then return True 
                 else g b as

checkPermDual :: (a -> a -> TCMonad Bool) -> [a] -> [a] -> TCMonad Bool
checkPermDual f as bs = do
    check1 <- checkPerm f as bs
    check2 <- checkPerm f bs as
    return $ check1 && check2

--type equality
equalBaseType :: BaseType -> BaseType -> TCMonad Bool
equalBaseType a b = 
  case (a,b) of
    (UnitType,UnitType) -> return True
    (BotType,BotType)   -> return True
    (PathType (Var n1), PathType (Var n2)) -> return $ n1 == n2
    (PathType (Field p1 n1), PathType (Field p2 n2)) -> do
      tau1 <- typecheckPath p1
      tau2 <- typecheckPath p2
      let eqPath = p1 == p2
      eqTy <- equalType tau1 tau2
      return $ (eqPath || eqTy) && (n1 == n2)
    _ -> return False  

equalType :: Type -> Type -> TCMonad Bool
equalType (Type b1 r1) (Type b2 r2) = do
  eqBase <- equalBaseType b1 b2
  eqRefs <- checkPermDual equalRef r1 r2
  return $ eqBase && eqRefs

equalRef :: Refinement -> Refinement -> TCMonad Bool
equalRef r1 r2 =
  case (r1,r2) of
    (ValRef b1 t1,ValRef b2 t2) -> do
      eqTy <- equalType t1 t2
      return $ b1 == b2 && eqTy
    (TypeRef (Binding t1 _) z1 r1,TypeRef (Binding t2 _) z2 r2) -> do
      let r2' = map (substRefines z2 (Var z1)) r2  
      eqRefs <- checkPermDual equalRef r1 r2'
      return $ t1 == t2 && eqRefs
    (DefRef (Binding b1 _) args1 t1,DefRef (Binding b2 _) args2 t2) -> do
      let types1 = map (\(Arg _ t) -> t) args1
      let types2 = map (\(Arg _ t) -> t) args2
      eqTypes <- checkPairwise equalType types1 types2
      return $ b1 == b2 && length args1 == length args2 && eqTypes
    (SubtypeRef s1 t1,SubtypeRef s2 t2) -> do
      eqs <- equalType s1 s2
      eqt <- equalType t1 t2
      return $ eqs && eqt
    (MemberRef (Binding b1 _) bound1 t1,MemberRef (Binding b2 _) bound2 t2) -> do
      eqTy <- equalType t1 t2
      return $ b1 == b2 && bound1 == bound2 && eqTy
    _ -> return False    

--subtyping 
isSubtype :: Type -> Type -> TCMonad Bool
isSubtype t1@(Type b1 r1) t2@(Type b2 r2) = do
  --throwError (show b1 ++ " :: " ++ show b2)
  eqBase <- equalBaseType b1 b2 
  return True
  {-
  if eqBase then checkPerm isSubtypeRef r1 r2 
            else do recl <- recLHS
                    recr <- recRHS
                    norm <- normalAns
                    return $ recl || recr || norm
  where 
    recLHS = return True
    recRHS = return True
    normalAns = do
      eqUnit <- equalType t2 theUnit
      let eqBot = case b1 of
                    BotType -> True
                    _       -> False
      baseCheck <- do
        isBase <- isSubtypeBase t1 b2
        if isBase then do
             (z,r1') <- unfold t1
             local (ValRef z t1:) $ checkPerm isSubtypeRef r1' r2
          else return False
      return $ eqUnit || eqBot || baseCheck
      -}
 
isSubtypeBase :: Type -> BaseType -> TCMonad Bool
isSubtypeBase t1 b2 = return True

isSubtypeRef :: Refinement -> Refinement -> TCMonad Bool
isSubtypeRef a b = return True
--WF checks
typeWF :: Type -> TCMonad Bool
typeWF tau = return True

refineWF :: Refinement -> TCMonad Bool
refineWF r = return True

subtypeWF :: Type -> Type -> TCMonad Bool
subtypeWF t1 t2 = return True
