module SymbolsTest
( Terminal(..)
, NonTerminal(..)
) where

-- system imports
import Test.QuickCheck

-- local imports
import Grammar.Regex


newtype Terminal = Terminal Char deriving (Eq, Ord)
instance Show Terminal where show (Terminal c) = [c]
instance ShowSymbol Terminal where showSymbol q (Terminal c) = quote q [c]
instance Arbitrary Terminal where
    arbitrary = Terminal <$> elements (['_', '+', '*'] ++ ['a'..'z'] ++ ['0'..'9'])

newtype NonTerminal = NonTerminal Char deriving (Eq, Ord)
instance Show NonTerminal where show (NonTerminal c) = [c]
instance ShowSymbol NonTerminal where showSymbol q (NonTerminal c) = quote q [c]
instance Arbitrary NonTerminal where
    arbitrary = NonTerminal <$> elements ['A'..'Z']
