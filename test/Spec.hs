import qualified CFGTest
import qualified CFGRandomTest
import qualified RegexTest
import qualified RegexRandomTest
import qualified MCTest

main :: IO ()
main = do
    _ <- MCTest.runTests
    _ <- RegexRandomTest.runTests
    _ <- RegexTest.runTests
    _ <- CFGTest.runTests
    _ <- CFGRandomTest.runTests
    return ()
