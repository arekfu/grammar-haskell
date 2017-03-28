module RegexRandomTest
( printExamples
, runTests
) where

-- system imports
import Test.QuickCheck
import Text.Regex.TDFA ((=~))
import Data.Foldable (forM_)
import Data.Coerce

-- local imports
import Grammar.MC
import Grammar.Regex
import Grammar.Regex.Random
import SymbolsTest (NonTerminal(..))
import RegexTest (ARegex(..))

printExamples :: IO ()
printExamples = do putStrLn "Performing random expansion of some random regexes..."
                   seeds <- sample' (arbitrary :: Gen (Positive (Large Seed)))
                   ars <- sample' (arbitrary :: Gen (ARegex NonTerminal))
                   forM_ ars $ \ar -> do
                     let r = unARegex ar
                     putStrLn $ "Expanding regex " ++ show r
                     forM_ (coerce seeds :: [Seed]) $ \seed ->
                         putStrLn $ concatMap show $ evalMC (randomExpandRegex r) seed

prop_randomExpansionMatches :: ARegex NonTerminal -> Positive (Large Seed) -> Property
prop_randomExpansionMatches (ARegex regex) (Positive (Large seed)) =
    let expanded :: String
        expanded = coerce $ evalMC (randomExpandRegex regex) seed
        tregex = showRegexWith NoQuoting (\(NonTerminal nt) -> [nt]) regex
        match :: Bool
        match = expanded =~ tregex
     in counterexample ("Counterexample: \"" ++ expanded ++ "\" does not match \"" ++ tregex ++ "\"")
          match


return []
runTests :: IO Bool
runTests = $quickCheckAll
