module TypeGraph where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Syntax
import PrettyPrint
import TypeUtil

data Context = Context {
                 typeAnnots :: [(BaseType,TypeAnnot)] --maps type to shape/material
               , bindings   :: [(Binding,BaseType)] --maps self-variables to their types
               , curPath    :: Maybe Path --Nothing if top-level, otherwise current path (used to get "absolute" basetype for type members)
               }
initCtx = Context [] [] Nothing

type TGMonad = WriterT [Edge] (ReaderT Context (Except String))

type Node = BaseType
data Edge = Edge {
              from  :: Node
            , label :: [Node] 
            , to    :: Node
            }

genEdges :: BaseType -> [Refinement] -> BaseType -> [Refinement] -> [Edge]
genEdges = undefined

getGraph prog = runExcept (
                  runReaderT (
                    execWriterT (buildGraph prog)
                  ) initCtx
                )

buildGraph :: Program -> TGMonad ()
buildGraph (Program decls expr) = f decls expr
  where f :: [Declaration] -> Expr -> TGMonad ()
        f [] expr = do
          return ()
        f (d:ds) expr = do
          buildGraphDecl d
          f ds expr

buildGraphDecl :: Declaration -> TGMonad () --return something useful
buildGraphDecl d = case d of
  TypeDecl ta ty z rs -> return ()  
  _ -> return ()

buildGraphExpr :: Expr -> TGMonad ()
buildGraphExpr = undefined
