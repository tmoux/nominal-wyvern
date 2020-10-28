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
data PType = 
    PTop
  | PBot
  | PVar Binding
  | PPath PType Name
  deriving (Eq, Ord)

instance Show PType where
  show PTop        = "Top"
  show PBot        = "Bot"
  show (PVar b)    = show b
  show (PPath p n) = printf "%s.%s" (show p) n

data SContext = SContext { 
                  pTypeToTA   :: Map.Map PType TypeAnnot
                , pathToPType :: Map.Map Path PType
                }
initSCtx = SContext Map.empty Map.empty

data Context = Context {
                 curPath    :: Maybe Path --Nothing if top-level, otherwise current path (used to get "absolute" basetype/path for type members)
               }
initCtx = Context Nothing

type TGMonad = WriterT [Edge] (ReaderT Context (StateT SContext (Except String)))

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
      --path' <- getPType (PathType path)
      return $ PPath path' name

updateCurPath :: Binding -> Context -> Context
updateCurPath b@(Binding name _) ctx =
  case (curPath ctx) of
    Nothing   -> ctx {curPath = Just (Var b)}
    Just path -> ctx {curPath = Just (Field path name)}
-----------
getGraph prog = runExcept (
                  evalStateT ( 
                    runReaderT (
                      execWriterT (buildGraph prog)
                    ) initCtx
                  ) initSCtx
                )

buildGraph :: Program -> TGMonad ()
buildGraph (Program decls expr) = f decls expr
  where f :: [Declaration] -> Expr -> TGMonad ()
        f [] expr = do
          return ()
        f (d:ds) expr = do
          buildGraphDecl d
          f ds expr

buildGraphDecl :: Declaration -> TGMonad () --return something useful?
buildGraphDecl d = case d of
  TypeDecl ta ty z rs -> do
    tyPath <- absType ty
    addPath (Var z) tyPath 
    addPType tyPath ta
    local (updateCurPath z) $ mapM_ rootGE (ref rs)
    where 
      rootGE (MemberRef ta t _ (Type bt rt)) = do
        bt' <- getPType bt
        nt  <- absType t
        addPType nt ta
        genEdges bt' rt nt []
  SubtypeDecl (Type n1 r1) n2 -> do
    n1' <- getPType n1
    n2' <- getPType n2
    tell [Edge n2' [] n1']
    mapM_ (recRefs n2') r1
    where recRefs n2' (MemberRef _ _ _ (Type nr rr)) = do
            nr' <- getPType nr
            tell [Edge n2' [] nr']
            mapM_ (recRefs n2') rr
          recRefs n2 _ = return () --ignore non type member refinements for now (?)
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
runCycleCheck es = runExcept (checkCycles es)

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
