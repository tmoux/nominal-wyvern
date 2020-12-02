module TypeGraph where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.List (union)
import Text.Printf
import Syntax
import PrettyPrint
import TypeUtil
import Debug.Trace

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

type Context = [TopLevelDeclaration]
initCtx = []

type TGMonad = WriterT [Edge] (ReaderT Context (Except String))

data Edge = Edge {
              from  :: PType
            , label :: [(PType,TypeAnnot)] 
            , to    :: PType
            }
  deriving (Show)
--state context (PTypes to material/shape)
lookupPType :: PType -> TGMonad TypeAnnot
lookupPType p = do
  val <- gets ((Map.lookup p) . pTypeToTA)
  case val of
    Just x -> return x
    Nothing -> throwError $ printf "failed to lookup ptype: %s" (show p)

lookupPath :: Path -> TGMonad PType --path to PType
lookupPath p = do
  val <- gets ((Map.lookup p) . pathToPType)
  case val of
    Just x  -> return x
    Nothing -> throwError $ printf "failed to lookup path: %s" (show p)

addPType :: PType -> TypeAnnot -> TGMonad ()
addPType p a = do
  sctx <- get
  let newmap = Map.insert p a (pTypeToTA sctx)
  put $ sctx {pTypeToTA = newmap}

addPath :: Path -> PType -> TGMonad ()
addPath p ap = do
  sctx <- get
  let newmap = Map.insert p ap (pathToPType sctx)
  put $ sctx {pathToPType = newmap}
--reader context (bindings to paths, curPath)
absPath :: Binding -> TGMonad Path
absPath b@(Binding name _) = do
  cur <- reader curPath
  case cur of
    Nothing   -> return $ Var b
    Just path -> return $ Field path name

absType :: Binding -> TGMonad PType
absType b@(Binding name _) = do
  cur <- reader curPath
  --traceM $ printf "curPath = %s, absType %s" (show cur) (show b)
  case cur of 
    Nothing   -> return $ PVar b
    Just path -> do
      --traceM $ "looking up " ++ (show path)
      pp <- lookupPath path
      return $ PPath pp name

getPType :: BaseType -> TGMonad PType
getPType base = case base of
  UnitType   -> return PTop
  BotType    -> return PBot
  PathType p -> case p of
    Var x           -> return $ PVar x
    Field path name -> do
      path' <- lookupPath path
      return $ PPath path' name

updateCurPath :: Binding -> Context -> Context
updateCurPath b@(Binding name _) ctx =
  case (curPath ctx) of
    Nothing   -> ctx {curPath = Just (Var b)}
    Just path -> ctx {curPath = Just (Field path name)}
--------------------------------------
getGraph prog = runExcept (
                  runReaderT (
                    execWriterT (buildGraph prog)
                  ) initCtx
                )

buildGraph :: Program -> TGMonad ()
buildGraph (Program decls expr) = local (const decls) $ do
  mapM_ buildGraphDecl decls

buildGraphDecl :: TopLevelDeclaration -> TGMonad ()
buildGraphDecl = undefined
-- Material shape separation:
-- A shape is never used as part of a lower bound syntactically (i.e. after ≥ or =).
-- The upper bound of a shape is always a shape, and named shapes can only subtype named shapes.
-- Shapes cannot be refined in refinements. <-- this one not added yet...

checkShapesNotInRefs :: Type -> TGMonad ()
checkShapesNotInRefs (Type _ rs) = mapM_ check rs
  where check (MemberRef ta t _ (Type bt rt)) = do
          bt' <- getPType bt
          btTA <- lookupPType bt'
          case btTA of
            Shape    -> invalidShape bt
            Material -> return ()
        check _ = return ()
        invalidShape shape = throwError $ printf "invalid shape usage: shape type %s used in refinement" (show shape)

buildGraphRef :: Refinement -> TGMonad ()
buildGraphRef r = case r of
  MemberRef ta t bound t2@(Type bt rt) -> do
    tyPath <- absType t
    addPType tyPath ta
    checkShapesNotInRefs t2
    bt' <- getPType bt
    nt  <- absType t
    addPType nt ta
    genEdges bt' rt nt []
    --check if shape is used as lower bound
    btTA <- lookupPType bt'
    case (bound,btTA) of
      (EQQ,Shape) -> throwError $ printf "invalid shape usage: %s used as lower bound" (show bt)
      (GEQ,Shape) -> throwError $ printf "invalid shape usage: %s used as lower bound" (show bt)
      _           -> return ()
    --If the type member is a shape, check that upper bound is a shape
    case (ta,bound,btTA) of
      (Shape,LEQ,Material) -> throwError $ printf "invalid shape usage: %s must be upper bounded by a shape" (show t)
      _ -> return ()
  SubtypeRef (Type n1 r1) n2 -> do
    n1' <- getPType n1
    n2' <- getPType n2
    tell [Edge n2' [] n1']
    mapM_ (recRefs n2') r1
    --If n1 is a shape, n2 must be a shape
    n1TA <- lookupPType n1'
    n2TA <- lookupPType n2'
    case (n1TA,n2TA) of
      (Shape,Material) -> throwError $ printf "invalid shape usage: %s can only subtype named shapes, but %s is not a shape" (show n1) (show n2)
      _ -> return ()
    where recRefs n2' (MemberRef _ _ _ (Type nr rr)) = do
            nr' <- getPType nr
            tell [Edge n2' [] nr']
            mapM_ (recRefs n2') rr
          recRefs n2 _ = return () --ignore non type member refinements for now (?)
  TypeRef ta ty z rs -> do
    tyPath <- absType ty
    addPType tyPath ta
    zpath <- absPath z
    addPath (Var z) tyPath 
    addPath zpath tyPath
    local (updateCurPath z) $ mapM_ buildGraphRef rs
  _ -> return ()

buildGraphDecl :: Declaration -> TGMonad () --return something useful?
buildGraphDecl d = case d of
  TypeDecl ta ty z rs -> do
    buildGraphRef (TypeRef ta ty z rs)
  SubtypeDecl t1 n2 -> do
    buildGraphRef (SubtypeRef t1 n2)
  ValDecl b (Type base _) expr -> do
    buildGraphExpr expr
    vpath <- absPath b
    pt    <- getPType base
    addPath vpath pt
  _ -> return ()

buildGraphExpr :: Expr -> TGMonad ()
buildGraphExpr e = do
  return ()

genEdges :: PType -> [Refinement] -> PType -> [(PType,TypeAnnot)] -> TGMonad ()
genEdges b [] br ba = tell [Edge br ba b]
genEdges b (MemberRef _ _ _ (Type bt rt):rest) br ba = do
  bt' <- getPType bt
  bTA <- lookupPType b
  genEdges bt' rt br ((b,bTA):ba)
  genEdges b rest br ba
genEdges _ _ _ _ = throwError "should never happen"

--naive cycle checking
checkCycles :: [Edge] -> Except String ()
checkCycles es = mapM_ (dfs es' []) vertices
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
