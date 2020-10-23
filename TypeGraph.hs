module TypeGraph where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.List (find)
import Text.Printf
import Syntax
import PrettyPrint
import TypeUtil
import Debug.Trace

--For clarity, PTypes are the nodes in the type graph
--They are like base types, but all path types/type members are given "absolute paths",
--such as A.S or A.B.T rather than z.S or z.B.T
data PType = 
    PTop
  | PBot
  | PPath Path
  deriving (Eq, Ord)

instance Show PType where
  show PTop      = "Top"
  show PBot      = "Bot"
  show (PPath p) = show p

type SContext = Map.Map PType TypeAnnot
initSCtx = Map.empty

data Context = Context {
                 bindings   :: Map.Map Binding Path --maps self-variables to their absolute path type
               , curPath    :: Maybe Path --Nothing if top-level, otherwise current path (used to get "absolute" basetype/path for type members)
               }
initCtx = Context Map.empty Nothing

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
  val <- gets (Map.lookup p)
  case val of
    Just x -> return x
    Nothing -> throwError $ printf "failed to lookup ptype: %s" (show p)

addPType :: PType -> TypeAnnot -> TGMonad ()
addPType p a = modify (Map.insert p a)
--reader context (bindings to paths, curPath)
lookupBinding :: Binding -> TGMonad Path
lookupBinding b = do
  val <- reader $ (Map.lookup b . bindings)
  case val of
    Just p  -> return p
    Nothing -> throwError $ printf "failed to lookup binding: %s" (show b)

absPath :: Binding -> TGMonad Path
absPath b@(Binding name _) = do
  cur <- reader curPath
  case cur of
    Nothing   -> return $ Var b
    Just path -> return $ Field path name

getPType :: BaseType -> TGMonad PType
getPType base = case base of
  UnitType   -> return PTop
  BotType    -> return PBot
  PathType p -> case p of
    Var x           -> return $ PPath (Var x)
    Field path name -> do
      path' <- f path
      return $ PPath (Field path' name)
  where 
    f p = case p of
      Var x           -> lookupBinding x
      Field path name -> do
        path' <- f path
        return $ Field path' name

addBinding :: Binding -> Path -> Context -> Context
addBinding b path ctx = ctx {bindings = Map.insert b path (bindings ctx)}

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
    tyPath <- absPath ty
    addPType (PPath tyPath) ta
    local ((addBinding z tyPath) . (updateCurPath ty)) $ 
      mapM_ rootGE (ref rs)
    where 
      rootGE (MemberRef ta t _ (Type bt rt)) = do
        bt' <- getPType bt
        nt  <- absPath t >>= (\p -> return (PPath p))
        addPType nt ta
        genEdges bt' rt nt []
  SubtypeDecl (Type n1 r1) (Type n2 _) -> do
    n1' <- getPType n1
    n2' <- getPType n2
    tell [Edge n2' [] n1']
    mapM_ (recRefs n2') r1
    where recRefs n2' (MemberRef _ _ _ (Type nr rr)) = do
            nr' <- getPType nr
            tell [Edge n2' [] nr']
            mapM_ (recRefs n2') rr
          recRefs n2 _ = return () --ignore non type member refinements for now (?)
  _ -> return ()

buildGraphExpr :: Expr -> TGMonad ()
buildGraphExpr = undefined

genEdges :: PType -> [Refinement] -> PType -> [(PType,TypeAnnot)] -> TGMonad ()
genEdges b [] br ba = tell [Edge br ba b]
genEdges b (MemberRef _ _ _ (Type bt rt):rest) br ba = do
  bt' <- getPType bt
  bTA <- lookupPType b
  genEdges bt' rt br ((b,bTA):ba)
  genEdges b rest br ba
genEdges _ _ _ _ = throwError "should never happen"
