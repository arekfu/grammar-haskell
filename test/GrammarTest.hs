{-# LANGUAGE TemplateHaskell #-}

module GrammarTest
( runTests
) where

-- system imports
import Test.QuickCheck
import qualified Data.IntMap as IM
import qualified Data.Vector.Unboxed as VU
import qualified Data.IntSet as IS

-- local imports
import Grammar.Internal

-- some properties to test the functions that manipulate IntMaps, IntSets, etc.

-- | All the map keys (nonterminals) and all the element of the sententials
--   must be found as symbols.
prop_collectSymbolsInclusion :: IM.IntMap [Sentence] -> Property
prop_collectSymbolsInclusion intMap =
    let syms = collectSymbols intMap
        keys = IM.keysSet intMap
        values = concat $ concat $ IM.elems intMap
     in keys `IS.isSubsetOf` syms .&&. all (`IS.member` syms) values

-- | The renumbering must conserve the number of rules and the number of
--   symbols.
prop_renumber :: IM.IntMap [Sentence] -> Property
prop_renumber intMap = let (intMap', renumbering, inverseRenumbering) = renumberMap intMap
                           nRules = IM.size intMap
                           nSyms = IS.size $ collectSymbols intMap
                        in nRules === IM.size intMap' .&&. nSyms === VU.length renumbering .&&. nSyms === IM.size inverseRenumbering


newtype Terminal = Terminal { terminalAsChar :: Char } deriving (Eq, Ord)
instance Show Terminal where show (Terminal c) = [c]
instance Arbitrary Terminal where
    arbitrary = Terminal <$> elements (['_', '+', '*'] ++ ['a'..'z'] ++ ['0'..'9'])

newtype NonTerminal = NonTerminal { nonTerminalAsChar :: Char } deriving (Eq, Ord)
instance Show NonTerminal where show (NonTerminal c) = [c]
instance Arbitrary NonTerminal where
    arbitrary = NonTerminal <$> elements ['A'..'Z']

newtype ACharCFG = ACharCFG CharCFG deriving (Show, Eq)
instance Arbitrary ACharCFG where
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
                   return $ ACharCFG $ productionsToCharCFG $ zip keys values


--prop_terminalsDisjointNonterminals :: ACharCFG -> Bool
--prop_terminalsDisjointNonterminals (ACharCFG g) = null $ getTerminals g `S.intersection` getNonTerminals g

symMin :: Int
symMin = -10

symMax :: Int
symMax = 10

newtype ASymbol = ASymbol Symbol deriving (Eq, Ord, Show)
instance Arbitrary ASymbol where
    arbitrary = ASymbol <$> elements [symMin..symMax]

maxSentenceLength :: Int
maxSentenceLength = 4

newtype ASentence = ASentence Sentence deriving (Eq, Ord, Show)
instance Arbitrary ASentence where
    arbitrary = do len <- elements [1..maxSentenceLength]
                   ASentence <$> vectorOf len arbitrary

unwrap :: (ASymbol, [ASentence]) -> (Symbol, [Sentence])
unwrap (ASymbol sym, sents) = (sym, map (\(ASentence sent) -> sent) sents)

newtype AIntCFG = AIntCFG IntCFG deriving (Eq, Ord, Show)
instance Arbitrary AIntCFG where
    arbitrary = do akvs <- arbitrary :: Gen [(ASymbol, [ASentence])]
                   let kvs = map unwrap akvs
                   let (grammar, _, _) = productionsToIntCFG kvs
                   return $ AIntCFG grammar


return []
runTests :: IO Bool
runTests = $quickCheckAll
