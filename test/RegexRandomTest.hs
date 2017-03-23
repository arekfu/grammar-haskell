module RegexRandomTest
( printExamples
) where

-- system imports
import Test.QuickCheck
import Data.Foldable (forM_)
import Data.Coerce

-- local imports
import Grammar.MC
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
                     forM_ ((coerce seeds)::[Seed]) $ \seed -> do
                         putStrLn $ concatMap show $ evalMC (randomExpandRegex r) seed
