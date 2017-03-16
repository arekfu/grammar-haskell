import qualified GrammarTest
import qualified RandomTest

main :: IO ()
main = do
    _ <- RandomTest.runTests
    _ <- GrammarTest.runTests
    return ()
