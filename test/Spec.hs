import qualified CFGTest
import qualified CFGRandomTest
import qualified RegexTest
import qualified RegexRandomTest
import qualified MCTest

main :: IO ()
main = do
    _ <- RegexRandomTest.printExamples
    _ <- RegexTest.printExamples
    _ <- MCTest.runTests
    _ <- RegexTest.runTests
    _ <- CFGTest.runTests
    _ <- CFGRandomTest.runTests
    return ()
