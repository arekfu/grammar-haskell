import qualified GrammarTest

main :: IO ()
main = do
    _ <- GrammarTest.runTests
    return ()
