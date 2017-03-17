import qualified GrammarTest
import qualified RandomTest

main :: IO ()
main = do
    _ <- GrammarTest.runTests
    _ <- RandomTest.runTests
    return ()
