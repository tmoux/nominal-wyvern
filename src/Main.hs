{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Environment
import System.Console.CmdArgs
import System.IO
import Text.ParserCombinators.Parsec
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

debug :: String -> String -> Int -> IO ()
debug header str 0 = return ()
debug header str x = putStrLn $ header ++ str

printList [] = ""
printList ls = foldr1 (\x y -> x ++ "\n" ++ y) (show <$> ls)

main = do
    arg <- cmdArgs wyv_args
    let infile = input arg
    let is_debug = debug_flag arg
    let is_repl = repl_mode arg
      
    prelude <- readFile "lib/Prelude.wyv"
    input <- readFile infile

    if is_repl then do
        repl [] [] 0
      else do
        let parse_res = parse parseProgram "" (prelude ++ input)
        let raw_ast = case parse_res of
                    Left err -> error (show err)
                    Right x -> x
        --putStrLn $ "Raw AST:\n" ++ show raw_ast
        let bound_ast = case bind raw_ast of
                    Left err -> error (show err)
                    Right x -> x
        putStrLn $ "Bound AST:\n" ++ show bound_ast
        let type_graph = case getGraph bound_ast of
                    Left err -> error (show err)
                    Right x -> x
        putStrLn $ "Type graph:\n" ++ printList type_graph
        case (runCycleCheck type_graph) of
                    Left err -> error (show err)
                    Right _ -> return ()
        let ty = case typecheck bound_ast of
                    Left err -> error (show err)
                    Right x -> x
        putStrLn $ "Type: " ++ (show ty)


--TODO
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
