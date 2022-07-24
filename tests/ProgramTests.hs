module ProgramTests where

import Binding (bind)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Trans.Except (except)
import Parser (parseFile)
import PrettyPrint ()
import Syntax (Type)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
    (@?=),
  )
import TypeGraph (checkCycles, getGraph)
import TypeUtil (theTop)
import Typecheck (typecheck)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft func (Left x) = Left (func x)
mapLeft _ (Right y) = Right y

runFile :: String -> String -> Either String Type
runFile prelude prog = runExcept asdf
  where
    asdf :: Except String Type
    asdf = do
      raw_ast <- except (mapLeft show (parseFile (prelude ++ prog)))
      bound_ast <- except (bind raw_ast)
      type_graph <- except (getGraph bound_ast)
      except (checkCycles type_graph)
      except (typecheck bound_ast)

-- Checks that a file parses and typechecks correctly, and that the final result type is equal to the expected type.
testResultType :: String -> String -> Assertion
testResultType file expected_type = do
  prelude <- readFile "lib/Prelude.wyv"
  input <- readFile file
  case runFile prelude input of
    Left err -> assertFailure err
    Right result_type -> show result_type @?= expected_type

-- Checks that attempting to typecheck a file results in an error.
testFailure :: String -> String -> Assertion
testFailure file expected_error = do
  prelude <- readFile "lib/Prelude.wyv"
  input <- readFile file
  case runFile prelude input of
    Left err -> err @?= expected_error
    Right _ -> assertFailure "Expected failure"

tests :: TestTree
tests =
  testGroup
    "Program tests"
    [ testCase "AB.wyv" $ testResultType "examples/AB.wyv" "Top",
      testCase "BadEQ.wyv" $ testResultType "examples/BadEq.wyv" "Top",
      testCase "eqEx.wyv" $ testResultType "examples/eqEx.wyv" "Set {type elemT = Fruit {type T = Fruit}}",
      testCase "loop.wyv" $ testFailure "examples/loop.wyv" "Cycle found: [C,A,B]"
    ]