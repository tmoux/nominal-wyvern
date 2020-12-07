{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module TypeGraph where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import Data.List (union)
import Text.Printf
import Syntax
import PrettyPrint
import TypeUtil
import Debug.Trace
import Typecheck (typecheckPath, typecheckExpr)

--For clarity, PTypes are the nodes in the type graph
--They are like base types, but all path types/type members are given "absolute paths",
--such as A.S or A.B.T rather than z.S or z.b.T
data PType
  = PTop
  | PBot
  | PVar Binding
  | PPath PType Name
  deriving (Eq, Ord)

instance Show PType where
  show PTop        = "Top"
  show PBot        = "Bot"
  show (PVar b)    = show b
  show (PPath p n) = printf "%s::%s" (show p) n

type PCtx = Map.Map PType TypeAnnot

type TGMonad = WriterT [Edge] (ReaderT Context (ReaderT PCtx (Except String)))

data Edge = Edge {
              from  :: PType
            , label :: [(PType,TypeAnnot)] 
            , to    :: PType
            }
  deriving (Show)

getPType :: BaseType -> TGMonad (PType,TypeAnnot)
getPType b = do
  pt <- convert b
  ta <- lookupPType pt
  return (pt,ta)
  where convert :: BaseType -> TGMonad PType
        convert b = case b of
          TopType      -> return PTop
          BotType      -> return PBot
          NamedType n  -> return (PVar n)
          PathType p t -> do
            Type b' _ <- typecheckPath p
            pt <- convert b'
            return (PPath pt t)

lookupPType :: PType -> TGMonad TypeAnnot
lookupPType pt = do
  pctx <- (lift.lift) ask
  case Map.lookup pt pctx of
    Just x  -> return x
    Nothing -> return Material
--------------------------------------
mapTAs :: [TopLevelDeclaration] -> Writer [(PType,TypeAnnot)] ()
mapTAs = mapM_ f
  where f (NameDecl ta n _ decls) = do
          tell [(PVar n,ta)]
          mapM_ (g n) decls
        f (SubtypeDecl _ _) = return ()
        g n (TypeMemDecl ta t _ _) = tell [(PPath (PVar n) t,ta)]
        g n _ = return ()

getGraph prog@(Program decls expr) = runExcept (
                  runReaderT (
                    runReaderT (
                      execWriterT (buildGraph prog)
                    ) (Context decls [] Off)
                  ) (Map.fromList (execWriter (mapTAs decls)))
                )

buildGraph :: Program -> TGMonad ()
buildGraph (Program decls expr) = do
  mapM_ buildGraphDecl decls
  buildGraphExpr expr

-- Material shape separation:
-- (1) A shape is never used as part of a lower bound syntactically (i.e. after ≥ or =).
-- (2) The upper bound of a shape is always a shape, and named shapes can only subtype named shapes.
-- (3) Shapes cannot be refined in refinements.

--check condition (3), that no shapes appear in any refinements
checkTy :: Type -> TGMonad ()
checkTy (Type _ rs) = mapM_ check rs
  where check (RefineDecl _ _ (Type bt rt)) = do
          (bt',btTA) <- getPType bt
          case btTA of
            Shape    -> invalidShape bt
            Material -> return ()
        invalidShape shape = throwError $ printf "invalid shape usage: shape type %s used in refinement" (show shape)

buildGraphDecl :: TopLevelDeclaration -> TGMonad ()
buildGraphDecl d = case d of
  NameDecl ta n z decls ->
    local (appendGamma [(z,makeNomType n)]) $ mapM_ (buildGraphMemDecl (PVar n)) decls
  SubtypeDecl t1@(Type n1 r1) n2 -> do
    checkTy t1
    (n1',n1TA) <- getPType n1
    (n2',n2TA) <- getPType n2
    tell [Edge n2' [] n1']
    mapM_ (recRefs n2') r1
    --If n1 is a shape, n2 must be a shape
    case (n1TA,n2TA) of
      (Shape,Material) -> throwError $ invalidShapeSubtype n1 n2
      _ -> return ()
    where recRefs n2' (RefineDecl _ _ (Type nr rr)) = do
            (nr',_) <- getPType nr
            tell [Edge n2' [] nr']
            mapM_ (recRefs n2') rr
          invalidShapeSubtype n1 n2 = printf "invalid shape usage: %s can only subtype named shapes, but %s is not a shape" (show n1) (show n2)

buildGraphMemDecl :: PType -> MemberDeclaration -> TGMonad ()
buildGraphMemDecl n d = case d of
  TypeMemDecl ta t bound t2@(Type bt rt) -> do
    checkTy t2
    let nt = PPath n t
    (bt',btTA) <- getPType bt
    genEdges (bt',btTA) rt nt []
    --check if shape is used as lower bound
    case (bound,btTA) of
      (EQQ,Shape) -> throwError $ invalidLB bt
      (GEQ,Shape) -> throwError $ invalidLB bt
      _           -> return ()
    --If the type member is a shape, check that upper bound is a shape
    case (ta,bound,btTA) of
      (Shape,LEQ,Material) -> throwError $ invalidShapeUB t
      _ -> return ()
    where invalidLB bt = printf "invalid shape usage: %s used as lower bound" (show bt)
          invalidShapeUB t = printf "invalid shape usage: %s must be upper bounded by a shape" (show t)
  ValDecl v ty -> checkTy ty
  DefDecl f args ty -> do
    mapM_ checkTy (map argType args)
    checkTy ty

buildGraphExpr :: Expr -> TGMonad ()
buildGraphExpr e = case e of
  PathExpr _ -> return ()
  Call _ _ _ -> return ()
  New ty z defns -> do
    checkTy ty
    local (appendGamma [(z,ty)]) $ mapM_ buildGraphDefns defns
  Let x annot e1 e2 -> do
    buildGraphExpr e1
    xTy <- case annot of
      Just ty -> return ty
      Nothing -> local turnSubtypingOff $ typecheckExpr e1
    local (appendGamma [(x,xTy)]) $ buildGraphExpr e2
  TopLit -> return ()
  UndefLit -> return ()

buildGraphDefns :: MemberDefinition -> TGMonad ()
buildGraphDefns d = case d of
  TypeMemDefn _ ty -> checkTy ty
  ValDefn _ ty _ -> checkTy ty
  DefDefn _ args ty expr -> local (appendGamma (map argToTup args)) $ do
    mapM_ checkTy (map argType args)
    checkTy ty
    buildGraphExpr expr
---------------------------------------
genEdges :: (PType,TypeAnnot) -> [Refinement] -> PType -> [(PType,TypeAnnot)] -> TGMonad ()
genEdges (b,_) [] br ba = tell [Edge br ba b]
genEdges (b,bTA) (RefineDecl _ _ (Type bt rt):rest) br ba = do
  bt' <- getPType bt
  genEdges (b,bTA) rest br ba
  genEdges bt' rt br ((b,bTA):ba)

--------------------------------------
--naive cycle checking
checkCycles :: [Edge] -> Either String ()
checkCycles es = runExcept $ mapM_ (dfs es' []) vertices
  where vertices = foldr (\(Edge from _ to) vs -> [from,to] `union` vs) [] es
        es' = filter noshape es
        noshape (Edge _ ls _) = and (map (isMat.snd) ls)
        isMat Material = True
        isMat Shape    = False

dfs :: [Edge] -> [PType] -> PType -> Except String ()
dfs es stack v =
  if v `elem` stack then
    throwError $ printf "Cycle found: %s" (show stack)
    else do
      let es' = filter (\(Edge from _ _) -> from == v) es
      mapM_ (\(Edge _ _ to) -> dfs es (v:stack) to) es'
