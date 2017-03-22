import qualified CFGTest
import qualified CFGRandomTest
import qualified RegexTest
import qualified RegexRandomTest

main :: IO ()
main = do
    _ <- RegexRandomTest.printExamples
    _ <- RegexTest.printExamples
    _ <- RegexTest.runTests
    _ <- CFGTest.runTests
    _ <- CFGRandomTest.runTests
    return ()
