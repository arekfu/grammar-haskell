module SymbolsTest
( Terminal(..)
, NonTerminal(..)
) where

-- system imports
import Test.QuickCheck


newtype Terminal = Terminal Char deriving (Eq, Ord)
instance Show Terminal where show (Terminal c) = [c]
instance Arbitrary Terminal where
    arbitrary = Terminal <$> elements (['_', '+', '*'] ++ ['a'..'z'] ++ ['0'..'9'])

newtype NonTerminal = NonTerminal Char deriving (Eq, Ord)
instance Show NonTerminal where show (NonTerminal c) = [c]
instance Arbitrary NonTerminal where
    arbitrary = NonTerminal <$> elements ['A'..'Z']
