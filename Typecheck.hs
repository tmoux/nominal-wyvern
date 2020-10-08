module Typecheck where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List (find)
import Syntax

data Error = OtherErr String
    deriving (Show)

type Context = [Refinement]
type TCMonad = ExceptT String (Reader Context)

lookupCtx :: (Refinement -> Bool) -> TCMonad Refinement
lookupCtx pred = do
    gamma <- ask
    case (find pred gamma) of
        Just x -> return x
        Nothing -> throwError "lookup failed"

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
        --TODO: subtype check
        return $ ValRef b ty
    DefDecl method args retTy prog -> do
        --TODO: add args to ctx and typecheck prog
        --local $ typecheckProgram prog
        return $ DefRef method args retTy        
    TypeDecl t z decls -> do
        let zt = ValRef z (makeNomType t)
        --rs <- local (zt:) $ 
        return $ TypeRef t z decls
    TypeEqDecl b ty -> do
        return $ MemberRef b EQQ ty
    SubtypeDecl t1 t2 -> do
        return $ SubtypeRef t1 t2

typecheckExpr :: Expr -> TCMonad Type
typecheckExpr e = case e of
    PathExpr p -> typecheckPath p
    New z ty decls -> do
        (z',rs) <- unfold ty
        let addtoCtx = (ValRef z ty:)
        refines <- mapM typecheckDecl decls
        let refines' = map (substRefines z (Var z')) refines
        --assert that new expr is structural subtype of ty
        checkPerm isSubtypeRef refines' rs
          >>= assert "invalid new expression"
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
        let subTy = subfunc retTy --correct return type
        let argTypes = map (\(Arg _ ty) -> ty) args
        let argsTypeSubbed = map subfunc argTypes --correct arg types
        esTys <- mapM typecheckPath es --calling types
        --subtype check
        checkPairwise isSubtype esTys argsTypeSubbed 
          >>= assert "Subtype check failed when calling method"
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
unfold (Type base rs) = case base of
    UnitType -> return (Binding "z" (-1),[]) --phony binding
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

assert :: String -> Bool -> TCMonad ()
assert err True = return ()
assert err False = throwError err

--check that f is true for all zipped pairs
checkPairwise :: (a -> b -> TCMonad Bool) -> [a] -> [b] -> TCMonad Bool
checkPairwise f as bs = foldM (\res (a,b) -> if res then f a b else return False) 
                              True 
                              (zip as bs)

--check that for all elem in b, there is an a s.t. (f a b)
checkPerm :: (a -> b -> TCMonad Bool) -> [a] -> [b] -> TCMonad Bool
checkPerm f as bs = 
  foldM (\res b -> if res then search b else return False)
        True
        bs
  where search b = g b as
        g b [] = return False
        g b (a:as) = do
          res <- f a b
          if res then return True 
                 else g b as

--subtyping 
isSubtype :: Type -> Type -> TCMonad Bool
isSubtype a b = case a of
    _ -> return True

isSubtypeRef :: Refinement -> Refinement -> TCMonad Bool
isSubtypeRef a b = return True
--WF checks
typeWF :: Type -> TCMonad Bool
typeWF tau = return True
