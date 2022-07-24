import qualified ProgramTests
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup
    "tests"
    [ ProgramTests.tests
    ]

main = defaultMain tests