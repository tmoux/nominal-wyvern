{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Environment
import System.Console.CmdArgs
import Text.ParserCombinators.Parsec
import Parser
import Binding
import PrettyPrint
import Typecheck

printList ls = foldr1 (\x y -> x ++ "\n" ++ y) (show <$> ls)

data Args = Generate { input :: FilePath
                     , debug_flag :: Int}
    deriving (Show, Data, Typeable)

generate = Generate { 
    input = "input.txt" &= typFile &= argPos 0
  , debug_flag = 0 &= typ "0/1"
                    } &= help ""
                      &= program "wyvern typechecker"

debug :: String -> String -> Int -> IO ()
debug header str 0 = return ()
debug header str x = putStrLn $ header ++ str

main = do
    args <- cmdArgs generate
    let infile = input args
    let dbg_flag = debug_flag args

    input <- readFile infile
    let parse_res = parse parseProgram "" input
    let raw_ast = case parse_res of
                Left err -> error (show err)
                Right x -> x
    --putStrLn $ "Raw AST:\n" ++ show raw_ast
    let bound_ast = case bind raw_ast of
                Left err -> error (show err)
                Right x -> x
    putStrLn $ "Bound AST:\n" ++ show bound_ast
    let ty = case typecheck bound_ast of
               Left err -> error (show err)
               Right x -> x
    putStrLn $ "Type: " ++ (show ty)
