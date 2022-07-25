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
    Left err -> assertFailure ("Expected success, but got error " ++ err)
    Right result_type -> show result_type @?= expected_type

-- Checks that attempting to typecheck a file results in an error.
testFailure :: String -> String -> Assertion
testFailure file expected_error = do
  prelude <- readFile "lib/Prelude.wyv"
  input <- readFile file
  case runFile prelude input of
    Left err -> err @?= expected_error
    Right result_type -> assertFailure ("Expected failure, but successfully got result type " ++ show result_type)

tests :: TestTree
tests =
  testGroup
    "Program tests"
    [ testCase "AB.wyv" $ testResultType "examples/AB.wyv" "Top",
      testCase "BadEQ.wyv" $ testResultType "examples/BadEq.wyv" "Top",
      testCase "eqEx.wyv" $ testResultType "examples/eqEx.wyv" "Set {type elemT = Fruit {type T = Fruit}}",
      testCase "graph.wyv" $ testResultType "examples/graph.wyv" "oog.e",
      testCase "Ipair.wyv" $ testResultType "examples/Ipair.wyv" "Top",
      -- note: this test demonstrates the limitations in expressivity of the current system.
      -- Once the new rules are implemented, this test should be updated.
      testCase "list.wyv" $ testResultType "examples/list.wyv" "Sum.T",
      testCase "loop.wyv" $ testFailure "examples/loop.wyv" "Cycle found: [C,A,B]",
      testCase "Module.wyv" $ testResultType "examples/Module.wyv" "Int",
      -- testCase "move.wyv" $ testResultType "examples/move.wyv" "FMoveable",
      testCase "NominalSubtypeTest.wyv" $ testResultType "examples/NominalSubtypeTest.wyv" "Top",
      testCase "Ordered.wyv" $ testResultType "examples/Ordered.wyv" "MyContainer",
      testCase "paths.wyv" $ testResultType "examples/paths.wyv" "cc.c.b.T",
      testCase "Set.wyv" $ testResultType "examples/Set.wyv" "ISet",
      testCase "test1.wyv" $ testResultType "examples/test1.wyv" "a.T"
    ]
