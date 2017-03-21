import qualified CFGTest
import qualified CFGRandomTest

main :: IO ()
main = do
    _ <- CFGTest.runTests
    _ <- CFGRandomTest.runTests
    return ()
