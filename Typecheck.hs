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

typecheck prog = runReader (runExceptT (typecheckProgram prog)) []

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
    TypeDecl t z decls -> do
        let zt = ValRef z (makeNomType t)
        --rs <- local (zt:) $ 
        return $ TypeRef t z decls
    TypeEq b ty -> do
        return $ MemberRef b EQQ ty
    _ -> throwError "TODO decl"

typecheckExpr :: Expr -> TCMonad Type
typecheckExpr e = case e of
    PathExpr p -> case p of
        Var b -> do
            ValRef _ ty <- lookupCtx pred
            return ty
            where pred (ValRef b' _) = b == b'
                  pred _ = False
        Field p n -> do
            tau <- typecheckExpr $ PathExpr p
            (z,decls) <- unfold tau
            ValRef _ tauv <- local (const decls) $ lookupCtx pred
            return $ substType z p tauv
            where pred (ValRef (b,_) _) = b == n
                  pred _ = False
    New z ty decls ->
        return ty
    UnitLit -> return theUnit
    _ -> throwError "TODO expr"

unfold :: Type -> TCMonad (Binding,[Refinement])
unfold (Type base rs) = case base of
    UnitType -> return (("z",-1),[]) --phony binding
    PathType p -> case p of
        Var x -> do
            TypeRef _ z rs <- lookupCtx pred
            return (z,rs)
            where pred (TypeRef b _ _) = b == x
                  pred _ = False
        Field pa na -> do
            tau <- typecheckExpr $ PathExpr pa
            (z,decls) <- unfold tau
            d <- local (const decls) $ lookupCtx pred
            case d of
              TypeRef _ z' rs -> return (z',map (substRefines z pa) rs)
              MemberRef _ bound ty -> case bound of
                GEQ -> return (("z",-1),[]) --unit binding
                _ -> unfold $ substType z pa ty
            where pred (TypeRef (b,_) _ _) = b == na
                  pred (MemberRef (b,_) _ _) = b == na 
                  pred _ = False
    _  -> throwError "unfold: shouldn't happen?"

assert :: String -> Bool -> TCMonad ()
assert err True = return ()
assert err False = throwError err

isSubtype :: Type -> Type -> TCMonad Bool
isSubtype a b = case a of
    _ -> return True

--testing
decls = [TypeDecl ("A",0) ("z",1) 
          [MemberRef ("T",2) LEQ theUnit, 
          ValRef ("f",3) (Type (PathType $ Field (Var ("z",1)) "T") [])
          ]
       , ValDecl ("a",3) $ New ("y",4) (makeNomType ("A",0)) 
          [TypeEq ("T",5) (makeNomType ("A",0)), 
           ValDecl ("f",6) UnitLit
          ]
        ]
expr = PathExpr (Field (Var ("a",3)) "f")
prog = Program decls expr
