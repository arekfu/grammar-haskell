import qualified CFGTest
import qualified CFGRandomTest
import qualified RegexTest
import qualified RegexRandomTest

main :: IO ()
main = do
    _ <- RegexRandomTest.runTests
    _ <- RegexTest.runTests
    _ <- CFGTest.runTests
    _ <- CFGRandomTest.runTests
    return ()
