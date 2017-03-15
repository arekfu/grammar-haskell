{-# LANGUAGE TemplateHaskell #-}

module GrammarTest
( runTests
) where

-- system imports
import Test.QuickCheck
import qualified Data.IntMap as IM
import qualified Data.Vector.Unboxed as VU
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Coerce

-- local imports
import Grammar.Internal

------------------------------------------------------------------------------------
--  some properties to test the functions that manipulate IntMaps, IntSets, etc.  --
------------------------------------------------------------------------------------

-- | All the map keys (nonterminals) and all the element of the sententials
--   must be found as labels.
prop_collectLabelsInclusion :: IM.IntMap [Sentence] -> Property
prop_collectLabelsInclusion intMap =
    let _labels = collectLabels intMap
        keys = IM.keysSet intMap
        values = concat $ concat $ IM.elems intMap
     in keys `IS.isSubsetOf` _labels .&&. all (`IS.member` _labels) values

-- | The renumbering must conserve the number of rules and the number of
--   labels.
prop_renumber :: Label -> IM.IntMap [Sentence] -> Property
prop_renumber start intMap = start `IM.member` intMap ==>
    let (intMap', renumbering, inverseRenumbering) = renumberMap start intMap
        nRules = IM.size intMap
        nLabels = IS.size $ collectLabels intMap
     in nRules === IM.size intMap'
           .&&. nLabels === VU.length renumbering
           .&&. nLabels === IM.size inverseRenumbering
           .&&. VU.head renumbering == start

-----------------------------------------
--  now some properties about IntCFGs  --
-----------------------------------------

labelMin :: Int
labelMin = -10

labelMax :: Int
labelMax = 10

newtype ALabel = ALabel Label deriving (Eq, Ord, Show)
instance Arbitrary ALabel where
    arbitrary = coerce <$> elements [labelMin..labelMax]

maxSentenceLength :: Int
maxSentenceLength = 4

newtype ASentence = ASentence Sentence deriving (Eq, Ord, Show)
instance Arbitrary ASentence where
    arbitrary = do len <- elements [1..maxSentenceLength]
                   coerce <$> vectorOf len (arbitrary :: Gen Label)

newtype AIntCFG = AIntCFG IntCFG deriving (Eq, Ord, Show)
instance Arbitrary AIntCFG where
    arbitrary = do akvs <- listOf1 arbitrary :: Gen [(ALabel, [ASentence])]
                   let kvs = coerce akvs
                   let start = fst $ head kvs
                   let (grammar, _, _) = productionsToIntCFG start kvs
                   return $ coerce grammar

prop_checkLabels :: AIntCFG -> Property
prop_checkLabels (AIntCFG (IntCFG maxLabel intMap)) =
    let _labels = collectLabels intMap
     in IS.findMin _labels === 0
        .&&. IS.findMax _labels === maxLabel
        .&&. IS.size _labels === maxLabel + 1


---------------------------------
--  properties about real CFG  --
---------------------------------

newtype Terminal = Terminal Char deriving (Eq, Ord)
instance Show Terminal where show (Terminal c) = [c]
instance Arbitrary Terminal where
    arbitrary = Terminal <$> elements (['_', '+', '*'] ++ ['a'..'z'] ++ ['0'..'9'])

newtype NonTerminal = NonTerminal Char deriving (Eq, Ord)
instance Show NonTerminal where show (NonTerminal c) = [c]
instance Arbitrary NonTerminal where
    arbitrary = NonTerminal <$> elements ['A'..'Z']

newtype ACharCFG = ACharCFG CharCFG deriving (Show, Eq)
instance Arbitrary ACharCFG where
    arbitrary = do Positive (Small terminalsSize) <- arbitrary :: Gen (Positive (Small Int))
                   Positive (Small nonTerminalsSize) <- arbitrary :: Gen (Positive (Small Int))
                   wTerminals <- vectorOf terminalsSize arbitrary :: Gen [Terminal]
                   wNonTerminals <- vectorOf nonTerminalsSize arbitrary :: Gen [NonTerminal]
                   let terminals = coerce wTerminals
                   let nonTerminals = coerce wNonTerminals
                   let start = head nonTerminals
                   let allLabels = terminals ++ nonTerminals
                   Positive grammarSize <- arbitrary :: Gen (Positive Int)
                   keys <- vectorOf grammarSize $ elements nonTerminals
                   values <- vectorOf grammarSize $ listOf1 $ listOf (elements allLabels)
                   return $ ACharCFG $ productionsToCharCFG start $ zip keys values


prop_terminalsDisjointNonterminals :: ACharCFG -> Bool
prop_terminalsDisjointNonterminals (ACharCFG g) = null $ getTerminals g `S.intersection` getNonTerminals g

prop_terminalsHaveNoProductions :: ACharCFG -> Bool
prop_terminalsHaveNoProductions (ACharCFG g) = all (null . productions g) $ getTerminals g

prop_nonTerminalsHaveProductions :: ACharCFG -> Bool
prop_nonTerminalsHaveProductions (ACharCFG g) = all (not . null . productions g) $ getNonTerminals g


return []
runTests :: IO Bool
runTests = $quickCheckAll
