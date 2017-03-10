{-|
Module      : Grammar.Internal
Description : Datatypes and main functions for CFG grammars.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD3
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the most important datatypes of the 'Grammar' package and
the functions that operate on them.
-}

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Grammar.Internal
(
-- * The 'Grammar' typeclass
  Grammar(..)
, pick
-- ** Pretty-printing parts of a grammar
-- $examplegrammar
, showSentence
, showProductions
, showGrammar
-- * Context-free grammars over alphabets of integers
, IntCFG
, productionsToIntCFG
-- * Context free grammars over alphabets of arbitrary types
, CFG
, productionsToCFG
, toLabel
, toSym
, unsafeToLabel
, unsafeToSym
, sentenceToSym
, sentencesToSym
, sentenceToLabel
, sentencesToLabel
-- * Context-free grammars over alphabets of specific types
, CharCFG
, productionsToCharCFG
, StringCFG
, productionsToStringCFG
) where

-- system imports
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Data.Maybe (maybeToList, fromJust)
import Data.Foldable (foldr')

{- |
A typeclass that specifies how grammar datatypes should behave. A production
rule is represented by a list of symbols. Alternative production rules
associated with the same symbol are represented as lists of lists.
-}
class Grammar g where
    -- | The type of the elements of the alphabet described by the grammar @g@.
    type Repr g :: *
    -- | @productions gr sym@ returns the productions associated with symbol @sym@ in grammar @gr@.
    productions :: g -> Repr g -> [[Repr g]]
    -- | Convert a symbol to a 'String'.
    showSymbol :: g -> Repr g -> String
    -- | Is this symbol part of the grammar?
    isInGrammar :: Repr g -> g -> Bool
    isInGrammar x gr = not $ isNotInGrammar x gr
    -- | Is this symbol part of the grammar?
    isNotInGrammar :: Repr g -> g -> Bool
    isNotInGrammar x gr = not $ isInGrammar x gr
    -- | Returns the set of all symbols used in the grammar
    getSymbols :: g -> S.Set (Repr g)
    -- | Returns the set of all terminals used in the grammar
    getTerminals :: g -> S.Set (Repr g)
    -- | Returns the set of all nonterminals used in the grammar
    getNonTerminals :: g -> S.Set (Repr g)

{- $examplegrammar #examplegrammar#
   We illustrate the pretty-printing functionality with the following example grammar:

   > exampleKeyValue :: [(Char, [String])]
   > exampleKeyValue = let initialChars = map (:[]) ['a'..'c']
   >                       chars = ['a'..'c'] ++ ['0'..'3']
   >                       expansions = map (\c -> ['I', c]) chars
   >                    in [ ('E', ["E+E", "E*E", "(E)", "I"])
   >                       , ('I', initialChars ++ expansions)
   >                       ]
   >
   > exampleGrammar :: CharCFG
   > exampleGrammar = productionsToCharCFG exampleKeyValue
-}


-- | Pretty-print a sentence (a sequence of symbols).
showSentence :: Grammar g
             => g           -- ^ the grammar
             -> [Repr g]    -- ^ the sentence
             -> String      -- ^ the sentence, pretty-printed as a String
showSentence grammar sent = concatMap (showSymbol grammar) sent

{- | Pretty-print all the production rules associated with a synbol.

   >>> putStrLn $ showProductions exampleGrammar 'E'
   E -> E+E
   E -> E*E
   E -> (E)
   E -> I
-}
showProductions :: Grammar g
                => g            -- ^ the grammar
                -> Repr g       -- ^ the symbol on the left-hand side of the rule
                -> String       -- ^ the pretty-printed production rule
showProductions grammar sym = let header = showSymbol grammar sym
                                  ls = productions grammar sym
                               in concatMap (\l -> header ++ " -> " ++ showSentence grammar l ++ "\n") ls

{- | Pretty-print all the production rules in a grammar.

   >>> putStrLn $ showGrammar exampleGrammar
   E -> E+E
   E -> E*E
   E -> (E)
   E -> I
   I -> a
   I -> b
   I -> c
   I -> Ia
   I -> Ib
   I -> Ic
   I -> I0
   I -> I1
   I -> I2
   I -> I3
-}
showGrammar :: Grammar g
            => g        -- ^ the grammar
            -> String   -- ^ its pretty-printed representation as a String
showGrammar grammar = let symbols = S.toList $ getSymbols grammar
                       in concatMap (showProductions grammar) symbols

-- | Pick the @n@-th production rule associated with a given symbol. WARNING:
--   unsafe if @n@ is out of bounds.
pick :: Grammar g
     => Int         -- ^ @n@, the rank of the selected rule
     -> g           -- ^ the grammar
     -> Repr g      -- ^ the nonterminal symbol
     -> [Repr g]    -- ^ the associated production rule
pick n grammar sym = productions grammar sym !! n

{- |
The IntCFG datatype implements a context-free grammar as a set of production
rules between strings of integers. The datatype requires the integers appearing
in the grammar to be consecutive, starting at @0@. This invariant is enforced
on construction (see 'productionsToIntCFG'). The production rules are
internally represented as an 'Data.IntMap.IntMap [Int]'. For efficiency
reasons, the 'CFG' datatype is built upon an 'IntCFG'.
-}

data IntCFG = IntCFG Int (IM.IntMap [[Int]]) deriving (Eq, Ord)

productionsInt :: IntCFG -> Int -> [[Int]]
productionsInt (IntCFG _ prods) nt = let inMap = IM.lookup nt prods
                                      in concat $ maybeToList inMap

isInIntCFG :: Int -> IntCFG -> Bool
isInIntCFG c (IntCFG n _) = c < n

isNotInIntCFG :: Int -> IntCFG -> Bool
isNotInIntCFG c (IntCFG n _) = c >= n

getSymbolsInt :: IntCFG -> S.Set Int
getSymbolsInt (IntCFG n _) = S.fromDistinctAscList [0..n-1]

getTerminalsInt :: IntCFG -> S.Set Int
getTerminalsInt gr = (getSymbolsInt gr) `S.difference` (getNonTerminalsInt gr)

getNonTerminalsInt :: IntCFG -> S.Set Int
getNonTerminalsInt (IntCFG _ prods) = S.fromList $ IM.keys prods

-- | Helper function.
insertInIntMap :: a -> (Int, IM.IntMap a) -> (Int, IM.IntMap a)
insertInIntMap c (n, labels) = (n+1, IM.insert n c labels)

-- | Helper function. Inverts keys and values.
invertIntMap :: IM.IntMap Int -> IM.IntMap Int
invertIntMap = IM.foldrWithKey' (\ key val inverse -> IM.insert val key inverse) IM.empty

-- | Helper function. Inverts keys and values.
invertMap :: Ord a => IM.IntMap a -> M.Map a Int
invertMap = IM.foldrWithKey' (\ key val inverse -> M.insert val key inverse) M.empty

-- | Build an 'IntCFG' from an association list of @(nonterminal, productions)@ pairs.
productionsToIntCFG :: [(Int, [[Int]])] -> (IntCFG, IM.IntMap Int, IM.IntMap Int)
productionsToIntCFG kvs = let (keys, values) = unzip kvs
                              uniqueKeys = S.fromList keys
                              uniqueValues = foldr' S.insert uniqueKeys $ concat $ concat values
                              uniqueLabels = uniqueKeys `S.union` uniqueValues
                              (maxSym, symsToLabels) = S.foldr' insertInIntMap (0, IM.empty) uniqueLabels
                              labelsToSyms = invertIntMap symsToLabels
                              prods = IM.fromList kvs
                           in ((IntCFG maxSym prods), symsToLabels, labelsToSyms)

instance Grammar IntCFG where
    type Repr IntCFG = Int
    productions = productionsInt
    showSymbol _ = show
    isInGrammar = isInIntCFG
    isNotInGrammar = isNotInIntCFG
    getSymbols = getSymbolsInt
    getTerminals = getTerminalsInt
    getNonTerminals = getNonTerminalsInt


{- | A datatype representing context-free grammars over alphabets of arbitrary
type. The implementation uses an 'IntCFG' grammar to represent the relations
between the symbols.

Conversion from __labels__ (i.e. objects of the external representation type,
@a@) to __symbols__ (i.e. 'Int's, the internal representation type) are
represented by a 'Data.Map.Map'; this implies that, for the most part, the type
variable @a@ is required to be in class 'Ord', although this constraint is not
enforced at the level of the datatype.

Conversion in the opposite direction (from __symbols__ to __labels__) are
represented by a 'Data.IntMap.IntMap'.
-}
data CFG a = CFG IntCFG (M.Map a Int) (IM.IntMap a)
             deriving (Eq, Ord)

-- | Convert a label to a symbol, given a dictionary.
toSym :: Ord a
      => M.Map a Int    -- ^ the label-to-symbol dictionary
      -> a              -- ^ the label to convert
      -> Maybe Int      -- ^ the resulting symbol, maybe
toSym dict label = M.lookup label dict

-- | Like 'toSym', but assume the label is in the dictionary.
unsafeToSym :: Ord a => M.Map a Int -> a -> Int
unsafeToSym dict = fromJust . (toSym dict)

-- | Convert a label to a symbol, given a dictionary.
toLabel :: IM.IntMap a  -- ^ the symbol-to-label dictionary
        -> Int          -- ^ the symbol to convert
        -> Maybe a      -- ^ the resulting label, maybe
toLabel dict sym = IM.lookup sym dict

-- | Like 'toLabel', but assume the label is in the dictionary.
unsafeToLabel :: Ord a => IM.IntMap a -> Int -> a
unsafeToLabel dict = fromJust . (toLabel dict)

-- | Convert a list of labels (a sentence) to a list of symbols. Labels that
--   do not belong to the grammar alphabet are silently expunged.
sentenceToSym :: Ord a => M.Map a Int -> [a] -> [Int]
sentenceToSym dict sentence = concatMap (maybeToList . (toSym dict)) sentence

-- | Convert a list of label sentences to a list of symbol sentences.
sentencesToSym :: Ord a => M.Map a Int -> [[a]] -> [[Int]]
sentencesToSym dict sentences = map (sentenceToSym dict) sentences

-- | Convert a list of symbols (a sentence) to a list of labels. Symbols that
--   do not belong to the grammar alphabet are silently expunged.
sentenceToLabel :: IM.IntMap a -> [Int] -> [a]
sentenceToLabel dict sentence = concatMap (maybeToList . (toLabel dict)) sentence

-- | Convert a list of symbol sentences to a list of label sentences.
sentencesToLabel :: IM.IntMap a -> [[Int]] -> [[a]]
sentencesToLabel dict sentences = map (sentenceToLabel dict) sentences

prodsToIntProds :: Ord a => M.Map a Int -> [(a, [[a]])] -> [(Int, [[Int]])]
prodsToIntProds dict ((k,vals):prods) = let k' = fromJust $ M.lookup k dict
                                            vals' = map (map (unsafeToSym dict)) vals
                                         in (k', vals'):prodsToIntProds dict prods
prodsToIntProds _ [] = []

{- | Build a 'CFG' from an association list of @(nonterminal, productions)@
     pairs. Here a production is a list of lists of symbols.

   > exampleKeyValue :: [(Char, [String])]
   > exampleKeyValue = let initialChars = map (:[]) ['a'..'c']
   >                       chars = ['a'..'c'] ++ ['0'..'3']
   >                       expansions = map (\c -> ['I', c]) chars
   >                    in [ ('E', ["E+E", "E*E", "(E)", "I"])
   >                       , ('I', initialChars ++ expansions)
   >                       ]
   >
   > exampleGrammar :: CharCFG
   > exampleGrammar = productionsToCharCFG exampleKeyValue
-}
productionsToCFG :: Ord a => [(a, [[a]])] -> CFG a
productionsToCFG kvs = let (keys, values) = unzip kvs
                           uniqueKeys = S.fromList keys
                           uniqueValues = foldr' S.insert uniqueKeys $ concat $ concat values
                           uniqueLabels = uniqueKeys `S.union` uniqueValues
                           (maxSym, symsToLabels) = S.foldr' insertInIntMap (0, IM.empty) uniqueLabels
                           labelsToSyms = invertMap symsToLabels
                           prods = IM.fromList $ prodsToIntProds labelsToSyms kvs
                        in CFG (IntCFG maxSym prods) labelsToSyms symsToLabels

productionsCFG :: Ord a => CFG a -> a -> [[a]]
productionsCFG (CFG iGr l2s s2l) label =
    case toSym l2s label of
        Nothing  -> []
        Just sym -> let prodsInt = productionsInt iGr sym
                     in sentencesToLabel s2l prodsInt

isInCFG :: Ord a => a -> CFG a -> Bool
isInCFG c (CFG iGr l2s _) = case toSym l2s c of
                                Nothing -> False
                                Just i  -> isInIntCFG i iGr

isNotInCFG :: Ord a => a -> CFG a -> Bool
isNotInCFG c (CFG iGr l2s _) = case toSym l2s c of
                                   Nothing -> False
                                   Just i  -> isNotInIntCFG i iGr

getSymbolsCFG :: Ord a => CFG a -> S.Set a
getSymbolsCFG (CFG iGr _ s2l) = let isyms = getSymbolsInt iGr
                                 in S.map (fromJust . toLabel s2l) isyms

getTerminalsCFG :: Ord a => CFG a -> S.Set a
getTerminalsCFG (CFG iGr _ s2l) = let isyms = getTerminalsInt iGr
                                   in S.map (fromJust . toLabel s2l) isyms

getNonTerminalsCFG :: Ord a => CFG a -> S.Set a
getNonTerminalsCFG (CFG iGr _ s2l) = let isyms = getNonTerminalsInt iGr
                                      in S.map (fromJust . toLabel s2l) isyms

instance (Ord a, Show a) => Grammar (CFG a) where
    type Repr (CFG a) = a
    productions = productionsCFG
    showSymbol _ = show
    isInGrammar = isInCFG
    isNotInGrammar = isNotInCFG
    getSymbols = getSymbolsCFG
    getTerminals = getTerminalsCFG
    getNonTerminals = getNonTerminalsCFG

instance (Ord a, Show a) => Show (CFG a) where show g = showGrammar g


{- | A newtype for 'Char'-based context-free grammars. This is solely done to
     improve the pretty-printing representation of the grammar symbols.
     Compare:

     >>> putStrLn $ showGrammar (productionsToCFG [('A', ["a"])])
     'A' -> 'a'
     >>> putStrLn $ showGrammar (productionsToCharCFG [('A', ["a"])])
     A -> a
-}
newtype CharCFG = CharCFG (CFG Char) deriving (Eq, Ord)

instance Grammar CharCFG where
    type Repr CharCFG = Char
    productions (CharCFG g) = productionsCFG g
    showSymbol _ s = [s]
    isInGrammar s (CharCFG g)= isInCFG s g
    isNotInGrammar s (CharCFG g) = isNotInCFG s g
    getSymbols (CharCFG g) = getSymbolsCFG g
    getTerminals (CharCFG g) = getTerminalsCFG g
    getNonTerminals (CharCFG g) = getNonTerminalsCFG g

-- | Build a 'CharCFG' from an association list of production rules -- see 'productionsToCFG'.
productionsToCharCFG :: [(Char, [String])] -> CharCFG
productionsToCharCFG = CharCFG . productionsToCFG

instance Show CharCFG where show g = showGrammar g

{- | A newtype for 'String'-based context-free grammars. This is solely done to
     improve the pretty-printing representation of the grammar symbols.
     Compare:

     >>> putStrLn $ showGrammar (productionsToCFG [("NONTERM", [["term"]])])
     "NONTERM" -> "term"
     >>> putStrLn $ showGrammar (productionsToStringCFG [("NONTERM", [["term"]])])
     NONTERM -> term
-}
newtype StringCFG = StringCFG (CFG String) deriving (Eq, Ord)

instance Grammar StringCFG where
    type Repr StringCFG = String
    productions (StringCFG g) = productionsCFG g
    showSymbol _ s = s
    isInGrammar s (StringCFG g)= isInCFG s g
    isNotInGrammar s (StringCFG g) = isNotInCFG s g
    getSymbols (StringCFG g) = getSymbolsCFG g
    getTerminals (StringCFG g) = getTerminalsCFG g
    getNonTerminals (StringCFG g) = getNonTerminalsCFG g

-- | Build a 'StringCFG' from an association list of production rules -- see 'productionsToCFG'.
productionsToStringCFG :: [(String, [[String]])] -> StringCFG
productionsToStringCFG = StringCFG . productionsToCFG

instance Show StringCFG where show g = showGrammar g
