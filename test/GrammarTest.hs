{-# LANGUAGE TemplateHaskell #-}

module GrammarTest
( runTests
) where

-- system imports
import Test.QuickCheck
import qualified Data.Set as S

-- local imports
import Grammar

newtype Terminal = Terminal { terminalAsChar :: Char } deriving (Eq, Ord)
instance Show Terminal where show (Terminal c) = [c]
instance Arbitrary Terminal where
    arbitrary = Terminal <$> elements (['_', '+', '*'] ++ ['a'..'z'] ++ ['0'..'9'])

newtype NonTerminal = NonTerminal { nonTerminalAsChar :: Char } deriving (Eq, Ord)
instance Show NonTerminal where show (NonTerminal c) = [c]
instance Arbitrary NonTerminal where
    arbitrary = NonTerminal <$> elements ['A'..'Z']

newtype ACSG = ACSG CharCFG deriving (Show, Eq)
instance Arbitrary ACSG where
    arbitrary = do Positive (Small terminalsSize) <- arbitrary :: Gen (Positive (Small Int))
                   Positive (Small nonTerminalsSize) <- arbitrary :: Gen (Positive (Small Int))
                   wTerminals <- vectorOf terminalsSize arbitrary :: Gen [Terminal]
                   wNonTerminals <- vectorOf nonTerminalsSize arbitrary :: Gen [NonTerminal]
                   let terminals = map terminalAsChar wTerminals
                   let nonTerminals = map nonTerminalAsChar wNonTerminals
                   let allSyms = terminals ++ nonTerminals
                   Positive grammarSize <- arbitrary :: Gen (Positive Int)
                   keys <- vectorOf grammarSize $ elements nonTerminals
                   values <- vectorOf grammarSize $ listOf $ listOf1 (elements allSyms)
                   return $ ACSG $ productionsToCharCFG $ zip keys values


prop_terminalsDisjointNonterminals :: ACSG -> Bool
prop_terminalsDisjointNonterminals (ACSG g) = null $ getTerminals g `S.intersection` getNonTerminals g

return []
runTests :: IO Bool
runTests = $quickCheckAll
