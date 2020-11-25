{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Environment
import System.Console.CmdArgs
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad.Except
import Control.Monad.Writer
import Parser
import Binding
import PrettyPrint
import TypeGraph
import Typecheck
import Syntax

data Args = Args { input :: FilePath
                 , debug_flag :: Bool
                 , repl_mode :: Bool} 
    deriving (Show, Data, Typeable)

wyv_args = Args { 
    input = def &= typFile &= argPos 0
  , debug_flag = def &= name "d"
  , repl_mode =  def &= name "r"
                    }   &= help ""
                        &= program "wyvern typechecker"


main = do
  arg <- cmdArgs wyv_args
  let infile = input arg
  let is_debug = debug_flag arg
  let is_repl = repl_mode arg
    
  prelude <- readFile "lib/Prelude.wyv"
  input <- readFile infile

  if is_repl then return ()
  else runFile (prelude ++ input)

runFile :: String -> IO ()
runFile input = do
  let raw_ast    = getRight $ parse parseProgram "" input
  let bound_ast  = getRight $ bind raw_ast
  putStrLn $ "Bound AST:\n" ++ show bound_ast

  let type_graph = getRight $ getGraph bound_ast 
  putStrLn $ "Type graph:\n" ++ printList type_graph
  let nocycles   = getRight $ runExcept (checkCycles type_graph)
  nocycles `seq` putStrLn $ "Type graph looks good"
  let ty         = getRight $ typecheck bound_ast 
  putStrLn $ "Type: " ++ (show ty)

getRight :: Show a => Either a b -> b
getRight e = case e of
  Left err -> error (show err)
  Right x  -> x
--TODO
{-
repl :: [Declaration] -> [BindCtx] -> Int -> IO ()
repl decls bindctx cnt = do
  putStr "> "
  hFlush stdout
  inp <- fmap (parse parseRepl "") getLine   
  case inp of
    Left err -> do
      putStrLn $ show err
      repl decls bindctx cnt
    Right x -> 
      case x of
        Quit -> putStrLn "Quitting."
        Reset -> do
          putStrLn "Resetting."
          repl [] [] 0
-}

printList [] = ""
printList ls = foldr1 (\x y -> x ++ "\n" ++ y) (show <$> ls)
