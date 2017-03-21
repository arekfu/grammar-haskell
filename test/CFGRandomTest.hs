module CFGRandomTest
( runTests
) where

-- system imports
import Prelude hiding (words)
import Data.Foldable (toList)
import Test.QuickCheck

-- local imports
import Grammar.CFG
import Grammar.CFG.Random
import Grammar.MC
import CFGTest (ACharCFG(..))

-- | Generator of random words given an alphabet
words :: [a] -> Gen [a]
words syms = listOf1 $ elements syms

-- | Terminals must not be expanded by the grammar rules
prop_terminalsAreInvariant :: ACharCFG -> Seed -> Property
prop_terminalsAreInvariant (ACharCFG g) seed =
    let terms = getTerminals g
     in conjoin $ map (\sym -> evalMC (randomSymExpand g sym) seed == [sym]) $ toList terms

-- | Deriving a word for 0 steps must be the identity
prop_deriveWord0IsId :: ACharCFG -> Seed -> Property
prop_deriveWord0IsId (ACharCFG g) seed =
    let syms = toList $ getSymbols g
     in not (null syms) ==> forAll (words syms) $ \word ->
            evalMC (randomWordDeriveN 0 g word) seed === word

-- | Deriving the starting symbol for 0 steps must be the identity
prop_deriveGrammar0IsId :: ACharCFG -> Seed -> Property
prop_deriveGrammar0IsId (ACharCFG g) seed =
    let syms = toList $ getSymbols g
     in not (null syms) ==> evalMC (randomGrammarDeriveN 0 g) seed === [startSymbol g]

return []
runTests :: IO Bool
runTests = $quickCheckAll
