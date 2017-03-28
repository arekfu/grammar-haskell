import qualified CFGTest
import qualified CFGParseTest
import qualified CFGRandomTest
import qualified RegexTest
import qualified RegexParseTest
import qualified RegexRandomTest
import qualified MCTest

main :: IO ()
main = do
    _ <- CFGParseTest.runTests
    _ <- MCTest.runTests
    _ <- RegexParseTest.runTests
    _ <- RegexRandomTest.runTests
    _ <- RegexTest.runTests
    _ <- CFGTest.runTests
    _ <- CFGRandomTest.runTests
    return ()
