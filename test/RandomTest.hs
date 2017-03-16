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

randomSentences :: [a] -> Gen [a]
randomSentences syms = listOf1 $ elements syms

prop_terminalsAreInvariant :: ACharCFG -> Seed -> Bool
prop_terminalsAreInvariant (ACharCFG g) seed =
    let terms = getTerminals g
     in all (\sym -> evalGrammar (randomSymExpand g sym) seed == [sym]) terms

prop_derive0IsId :: ACharCFG -> Seed -> Property
prop_derive0IsId (ACharCFG g) seed =
    let syms = toList $ getSymbols g
     in not (null syms) ==> forAll (randomSentences syms) $ \sent ->
            evalGrammar (randomSentDeriveN 0 g sent) seed === sent

prop_deriveNYieldsNItems :: ACharCFG -> Seed -> Positive Int -> Property
prop_deriveNYieldsNItems (ACharCFG g) seed (Positive n) =
    let syms = toList $ getSymbols g
     in not (null syms) ==> forAll (randomSentences syms) $ \sent ->
            length (evalGrammar (randomSentDeriveN n g sent) seed) <= n

return []
runTests :: IO Bool
runTests = $quickCheckAll
