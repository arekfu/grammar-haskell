{-# LANGUAGE TemplateHaskell #-}

module RandomTest
( runTests
) where

-- system imports
import Data.Foldable (all, toList)
import Test.QuickCheck

-- local imports
import Grammar.Internal
import Grammar.Random
import GrammarTest (ACharCFG(..))

-- | Generator of random sentences given an alphabet
randomSentences :: [a] -> Gen [a]
randomSentences syms = listOf1 $ elements syms

-- | Terminals must not be expanded by the grammar rules
prop_terminalsAreInvariant :: ACharCFG -> Seed -> Bool
prop_terminalsAreInvariant (ACharCFG g) seed =
    let terms = getTerminals g
     in all (\sym -> evalGrammar (randomSymExpand g sym) seed == [sym]) terms

-- | Deriving a sentence for 0 steps must be the identity
prop_deriveSent0IsId :: ACharCFG -> Seed -> Property
prop_deriveSent0IsId (ACharCFG g) seed =
    let syms = toList $ getSymbols g
     in not (null syms) ==> forAll (randomSentences syms) $ \sent ->
            evalGrammar (randomSentDeriveN 0 g sent) seed === sent

-- | Deriving the starting symbol for 0 steps must be the identity
prop_deriveGrammar0IsId :: ACharCFG -> Seed -> Property
prop_deriveGrammar0IsId (ACharCFG g) seed =
    let syms = toList $ getSymbols g
     in not (null syms) ==> evalGrammar (randomGrammarDeriveN 0 g) seed === [startSymbol g]

return []
runTests :: IO Bool
runTests = $quickCheckAll
