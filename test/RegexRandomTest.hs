module RegexRandomTest
( runTests
) where

-- system imports
import Test.QuickCheck
import Data.Foldable (forM_)

-- local imports
import Grammar.MC
import Grammar.Regex.Random
import CFGTest (NonTerminal)
import RegexTest (ARegex(..))

runTests :: IO ()
runTests = do putStrLn "Performing random expansion of some random regexes..."
              seeds <- sample' (arbitrary :: Gen Seed)
              ars <- sample' (arbitrary :: Gen (ARegex NonTerminal))
              forM_ ars $ \ar -> do
                let r = unARegex ar
                putStrLn $ "Expanding regex " ++ show r
                forM_ seeds $ \seed -> do
                    putStrLn $ concatMap show $ evalMC (randomExpandRegex r) seed
