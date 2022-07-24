{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Binding                (bind)
import           Parser                 (parseFile)
import           PrettyPrint            ()
import           System.Console.CmdArgs
import           System.Environment
import           System.IO
import           TypeGraph              (checkCycles, getGraph)
import           Typecheck              (typecheck)

data Args = Args
  { input      :: FilePath
  , debug_flag :: Bool
  , repl_mode  :: Bool
  }
  deriving (Show, Data, Typeable)

wyv_args =
  Args { input      = def &= typFile &= argPos 0
       , debug_flag = def &= name "d"
       , repl_mode  = def &= name "r"
       }
    &= help ""
    &= program "wyvern typechecker"

main = do
  arg <- cmdArgs wyv_args
  let infile   = input arg
  let is_debug = debug_flag arg
  let is_repl  = repl_mode arg

  prelude <- readFile "lib/Prelude.wyv"
  input   <- readFile infile

  if is_repl then return () else runFile (prelude ++ input)

runFile :: String -> IO ()
runFile input = do
  let raw_ast = case parseFile input of
        Left  err -> error (show err)
        Right x   -> x
  --putStrLn $ "Raw AST:\n" ++ show raw_ast
  let bound_ast = getRight $ bind raw_ast
  putStrLn $ "Bound AST:\n" ++ show bound_ast

  let type_graph = getRight $ getGraph bound_ast
  putStrLn "Type graph:" >> mapM_ (putStrLn . show) type_graph
  let nocycles = getRight $ checkCycles type_graph
  nocycles `seq` putStrLn $ "Type graph looks good"
  let ty = getRight $ typecheck bound_ast
  putStrLn $ "Type: " ++ (show ty)

getRight :: Either String b -> b
getRight e = case e of
  Left  err -> error err
  Right x   -> x
