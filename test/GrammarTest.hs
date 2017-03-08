{-# LANGUAGE TemplateHaskell #-}

module GrammarTest
( runTests
) where

-- system imports
import Test.QuickCheck
import qualified Data.Set as S

-- local imports
import Grammar

newtype ASCIIChar = ASCIIChar Char deriving (Eq, Ord)
instance Show ASCIIChar where show (ASCIIChar c) = [c]
instance Arbitrary ASCIIChar where
    arbitrary = ASCIIChar <$> (elements $ '_':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])


newtype ASymbol = AS { getASymbol :: Symbol ASCIIChar } deriving (Show, Eq)
instance Arbitrary ASymbol where
    arbitrary = AS <$> (S <$> arbitrary)

newtype ASentention = ASentention (Sentention ASCIIChar) deriving (Show, Eq)
instance Arbitrary ASentention where
    arbitrary = do asyms <- listOf arbitrary
                   let syms = map getASymbol asyms
                   return $ ASentention $ Sentention syms

newtype ACSG = ACSG (CSG ASCIIChar) deriving (Show, Eq)
instance Arbitrary ACSG where
    arbitrary = do terminals <- listOf1 $ elements $ map ASCIIChar (['a'..'z'] ++ ['+', '*', '/', '-'])
                   nonterms <- listOf1 $ elements $ map ASCIIChar ['A'..'Z']
                   let allSyms = terminals ++ nonterms
                   grammarSize <- arbitrary :: Gen Int
                   keys <- vectorOf grammarSize $ elements nonterms
                   values <- vectorOf grammarSize $ listOf $ listOf1 (elements allSyms)
                   return $ ACSG $ productionsToGrammar $ zip keys values


prop_terminalsDisjointNonterminals :: ACSG -> Bool
prop_terminalsDisjointNonterminals (ACSG g) = null $ (alphabet g) `S.intersection` (nonterminals g)

return []
runTests :: IO Bool
runTests = $quickCheckAll
