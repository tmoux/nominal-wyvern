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

  if is_repl then do
    return ()
  else 
    case runExcept $ execWriterT $ runFile (prelude ++ input) of
      Left err -> error err
      Right x -> putStrLn x

runFile :: String -> WriterT String (Except String) ()
runFile input = do
  let parse_res = parse parseProgram "" input
  raw_ast <- case parse_res of
    Left err -> throwError (show err)
    Right x -> return x
  bound_ast <- lift $ bind raw_ast
  type_graph <- lift $ getGraph bound_ast 
  lift $ checkCycles type_graph
  ty <- lift $ typecheck bound_ast 
  --tell $ "Raw AST:\n" ++ show raw_ast
  tell $ "Bound AST:\n" ++ show bound_ast ++ "\n"
  tell $ "Type graph:\n" ++ printList type_graph ++ "\n"
  tell $ "Type: " ++ (show ty) ++ "\n"

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
