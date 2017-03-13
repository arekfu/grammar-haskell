{-|
Module      : Grammar.Internal
Description : Datatypes and main functions for CFG grammars.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD3
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the most important datatypes of the package and the
functions that operate on them.
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
, IntCFG(..)
-- ** Type synonims
, Label
, Sentence
, Renumbering
, InverseRenumbering
-- ** Manipulation functions
, productionsToIntMap
, intMapToIntCFG
, productionsToIntCFG
, renumberLabel
, renumberSentence
, inverseRenumberLabel
, inverseRenumberSentence
, collectLabels
, renumberMap
-- * Context free grammars over alphabets of arbitrary types
, CFG(..)
, productionsToCFG
, toSymbol
, toLabel
, unsafeToSymbol
, unsafeToLabel
, sentenceToLabel
, sentencesToLabel
, sentenceToSymbol
, sentencesToSymbol
-- * Context-free grammars over alphabets of specific types
, CharCFG(..)
, productionsToCharCFG
, StringCFG(..)
, productionsToStringCFG
) where

-- system imports
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Data.Maybe (maybeToList, mapMaybe, fromJust)
import Data.Foldable (foldr')
import qualified Data.Vector.Unboxed as VU

{- |
A typeclass that specifies how grammar datatypes should behave. A production
rule is represented by a list of labels. Alternative production rules
associated with the same label are represented as lists of lists.
-}
class Grammar g where
    -- | The type of the elements of the alphabet described by the grammar @g@.
    type Repr g :: *
    -- | @productions gr sym@ returns the productions associated with label @sym@ in grammar @gr@.
    productions :: g -> Repr g -> [[Repr g]]
    -- | Convert a label to a 'String'.
    showLabel :: g -> Repr g -> String
    -- | Is this label part of the grammar?
    isInGrammar :: Repr g -> g -> Bool
    isInGrammar x gr = not $ isNotInGrammar x gr
    -- | Is this label part of the grammar?
    isNotInGrammar :: Repr g -> g -> Bool
    isNotInGrammar x gr = not $ isInGrammar x gr
    -- | Returns the set of all labels used in the grammar
    getLabels :: g -> S.Set (Repr g)
    -- | Returns the set of all terminals used in the grammar
    getTerminals :: g -> S.Set (Repr g)
    -- | Returns the set of all nonterminals used in the grammar
    getNonTerminals :: g -> S.Set (Repr g)


-- | Pretty-print a sentence (a sequence of labels).
showSentence :: Grammar g
             => g           -- ^ the grammar
             -> [Repr g]    -- ^ the sentence
             -> String      -- ^ the sentence, pretty-printed as a String
showSentence grammar = concatMap (showLabel grammar)

{- | Pretty-print all the production rules associated with a synbol.

   >>> putStrLn $ showProductions exampleGrammar 'E'
   E -> E+E
   E -> E*E
   E -> (E)
   E -> I
-}
showProductions :: Grammar g
                => g            -- ^ the grammar
                -> Repr g       -- ^ the label on the left-hand side of the rule
                -> String       -- ^ the pretty-printed production rule
showProductions grammar label = let header = showLabel grammar label
                                    ls = productions grammar label
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
showGrammar grammar = let labels = S.toList $ getLabels grammar
                       in concatMap (showProductions grammar) labels

-- | Pick the @n@-th production rule associated with a given label. WARNING:
--   unsafe if @n@ is out of bounds.
pick :: Grammar g
     => Int         -- ^ @n@, the rank of the selected rule
     -> g           -- ^ the grammar
     -> Repr g      -- ^ the nonterminal label
     -> [Repr g]    -- ^ the associated production rule
pick n grammar label = productions grammar label !! n

{- |
The IntCFG datatype implements a context-free grammar as a set of production
rules between strings of integers. The datatype requires the integers appearing
in the grammar to be consecutive, starting at @0@. This invariant is enforced
on construction (see 'productionsToIntCFG'). The production rules are
internally represented as an 'Data.IntMap.IntMap [[Int]]'. For efficiency
reasons, the 'CFG' datatype is built upon an 'IntCFG'.
-}

data IntCFG = IntCFG Label (IM.IntMap [Sentence]) deriving (Eq, Ord)

-- | Type synonim for 'Int'.
type Label = Int

-- | Type synonim for a list of 'Int's.
type Sentence = [Int]

productionsInt :: IntCFG -> Label -> [Sentence]
productionsInt (IntCFG _ prods) nt = let inMap = IM.lookup nt prods
                                      in concat $ maybeToList inMap

isInIntCFG :: Label -> IntCFG -> Bool
isInIntCFG c (IntCFG n _) = c < n

isNotInIntCFG :: Label -> IntCFG -> Bool
isNotInIntCFG c (IntCFG n _) = c >= n

getLabelsInt :: IntCFG -> S.Set Label
getLabelsInt (IntCFG n _) = S.fromDistinctAscList [0..n-1]

getTerminalsInt :: IntCFG -> S.Set Label
getTerminalsInt gr = getLabelsInt gr `S.difference` getNonTerminalsInt gr

getNonTerminalsInt :: IntCFG -> S.Set Label
getNonTerminalsInt (IntCFG _ prods) = S.fromList $ IM.keys prods

-- | Helper function.
insertInIntMap :: a -> (Label, IM.IntMap a) -> (Label, IM.IntMap a)
insertInIntMap c (n, labels) = (n+1, IM.insert n c labels)

-- | Helper function. Inverts keys and values.
invertMap :: Ord a => IM.IntMap a -> M.Map a Label
invertMap = IM.foldrWithKey' (\ key val inverse -> M.insert val key inverse) M.empty

-- | Type synonim for a 'Data.Vector.Unboxed.Vector' of 'Int's.
type Renumbering = VU.Vector Int

-- | Type synonim for a 'Data.IntMap.IntMap' of 'Int's.
type InverseRenumbering = IM.IntMap Int

-- | Invert an 'Int'-to-'Int' renumbering.
invert :: Renumbering -> InverseRenumbering
invert = VU.ifoldr' (\i label inverse -> IM.insert label i inverse) IM.empty

-- | Apply the renumbering to a given 'Label'.
renumberLabel :: Renumbering -> Label -> Label
renumberLabel renumb label = renumb VU.! label

-- | Apply the renumbering to a given 'Sentence'
renumberSentence :: Renumbering -> Sentence -> Sentence
renumberSentence renumb = map (renumberLabel renumb)

-- | Apply the inverse renumbering to a given 'Label'
inverseRenumberLabel :: InverseRenumbering -> Label -> Label
inverseRenumberLabel iRenumb label = fromJust $ IM.lookup label iRenumb

-- | Apply the inverse renumbering to a given 'Sentence'
inverseRenumberSentence :: InverseRenumbering -> Sentence -> Sentence
inverseRenumberSentence iRenumb = map (inverseRenumberLabel iRenumb)

-- | Return all labels appearing in a set of (integer) production rules.
collectLabels :: IM.IntMap [Sentence] -> IS.IntSet
collectLabels =
    let insertSentences :: IS.IntSet -> [Sentence] -> IS.IntSet
        insertSentences set sentences = foldr' IS.insert set $ concat sentences
     in IM.foldrWithKey (\key values set -> insertSentences (IS.insert key set) values) IS.empty

-- | Renumber the @n@ integers that appear as keys and values of the map in
--   such a way that they exactly span the @[0,n-1]@ interval. The
--   correspondence between the new and the old numbering scheme
--   ('Renumbering') is returned as a 'Data.Vector.Unboxed.Vector'. The inverse
--   mapping is returned as an 'Data.IntMap.IntMap'.
renumberMap :: IM.IntMap [Sentence] -- ^ The map to renumber
            -> (IM.IntMap [Sentence], Renumbering, InverseRenumbering)  -- ^ The renumbered map, the new-to-old mapping and the old-to-new mapping
renumberMap intMap = let allLabels = collectLabels intMap
                         renumbering = VU.fromList $ IS.toList allLabels
                         inverseRenumbering = invert renumbering
                         intMap' = IM.mapKeys (inverseRenumberLabel inverseRenumbering) intMap
                         intMap'' = IM.map (map (inverseRenumberSentence inverseRenumbering)) intMap'
                      in (intMap'', renumbering, inverseRenumbering)

{- | Build an 'IntCFG' from an association list of @(nonterminal, productions)@
     pairs. The Labels appearing in the association list are renumbered to
     enforce the invariant that they span the @[0,n-1]@ range. The renumbering
     is returned along with the built grammar, in the form of a
     'Data.Vector.Unboxed.Vector' 'Label' indexed by the renumbered labels.
-}
productionsToIntCFG :: [(Label, [Sentence])] -> (IntCFG, Renumbering, InverseRenumbering)
productionsToIntCFG = intMapToIntCFG . productionsToIntMap

{- | Build an 'IntCFG' from an 'Data.IntMap.IntMap' between 'Label's. The
     'Labels' will be renumbered as specified in the second and third return
     values.
-}
intMapToIntCFG :: IM.IntMap [Sentence] -> (IntCFG, Renumbering, InverseRenumbering)
intMapToIntCFG intMap = let (intMap', renumbering, inverseRenumbering) = renumberMap intMap
                            maxLabel = VU.length renumbering - 1
                         in (IntCFG maxLabel intMap', renumbering, inverseRenumbering)

{- | Provide the fundamental building block to construct an 'IntCFG' from an
     association list of @(nonterminal, productions)@ pairs. The Labels are
     not guaranteed to span the @[0,n-1]@ range.
-}
productionsToIntMap :: [(Label, [Sentence])] -> IM.IntMap [Sentence]
productionsToIntMap = IM.fromList

instance Grammar IntCFG where
    type Repr IntCFG = Label
    productions = productionsInt
    showLabel _ = show
    isInGrammar = isInIntCFG
    isNotInGrammar = isNotInIntCFG
    getLabels = getLabelsInt
    getTerminals = getTerminalsInt
    getNonTerminals = getNonTerminalsInt

instance Show IntCFG where show = showGrammar


{- | A datatype representing context-free grammars over alphabets of arbitrary
type. The implementation uses an 'IntCFG' grammar to represent the relations
between the labels.

Conversion from __symbols__ (i.e. objects of the external representation type,
@a@) to __labels__ (i.e. 'Int's, the internal representation type) are
represented by a 'Data.Map.Map'; this implies that, for the most part, the type
variable @a@ is required to be in class 'Ord', although this constraint is not
enforced at the level of the datatype.

Conversion in the opposite direction (from __labels__ to __symbols__) are
represented by a 'Data.IntMap.IntMap'.
-}
data CFG a = CFG IntCFG (M.Map a Label) (IM.IntMap a)
             deriving (Eq, Ord)

-- | Convert a symbol to a label, given a dictionary.
toLabel :: Ord a
          => M.Map a Label -- ^ the symbol-to-label dictionary
          -> a              -- ^ the symbol to convert
          -> Maybe Label    -- ^ the resulting label, maybe
toLabel dict symbol = M.lookup symbol dict

-- | Like 'toLabel', but assume the symbol is in the dictionary.
unsafeToLabel :: Ord a => M.Map a Label -> a -> Label
unsafeToLabel dict = fromJust . toLabel dict

-- | Convert a label to a symbol, given a dictionary.
toSymbol :: IM.IntMap a  -- ^ the label-to-symbol dictionary
        -> Label       -- ^ the label to convert
        -> Maybe a      -- ^ the resulting symbol, maybe
toSymbol dict sym = IM.lookup sym dict

-- | Like 'toSymbol', but assume the label is in the dictionary.
unsafeToSymbol :: Ord a => IM.IntMap a -> Label -> a
unsafeToSymbol dict = fromJust . toSymbol dict

-- | Convert a string of symbols to a list of labels. Symbols that do not
--   belong to the grammar alphabet are silently expunged.
sentenceToLabel :: Ord a => M.Map a Label -> [a] -> Sentence
sentenceToLabel dict = mapMaybe (toLabel dict)

-- | Convert a list of symbol strings to a list of label sentences.
sentencesToLabel :: Ord a => M.Map a Label -> [[a]] -> [Sentence]
sentencesToLabel dict = map (sentenceToLabel dict)

-- | Convert a list of labels (a sentence) to a string of symbols. Labels
--   that do not belong to the grammar alphabet are silently expunged.
sentenceToSymbol :: IM.IntMap a -> Sentence -> [a]
sentenceToSymbol dict = mapMaybe (toSymbol dict)

-- | Convert a list of label sentences to a list of symbol strings.
sentencesToSymbol :: IM.IntMap a -> [Sentence] -> [[a]]
sentencesToSymbol dict = map (sentenceToSymbol dict)

prodsToIntProds :: Ord a => M.Map a Label -> [(a, [[a]])] -> [(Label, [Sentence])]
prodsToIntProds dict ((k,vals):prods) = let k' = fromJust $ M.lookup k dict
                                            vals' = map (map (unsafeToLabel dict)) vals
                                         in (k', vals'):prodsToIntProds dict prods
prodsToIntProds _ [] = []

{- | Build a 'CFG' from an association list of @(nonterminal, productions)@
     pairs. Here a production is a list of lists of labels.

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
                           uniqueSymbols = uniqueKeys `S.union` uniqueValues
                           (nextLabel, labelsToSymbols) = S.foldr' insertInIntMap (0, IM.empty) uniqueSymbols
                           maxLabel = nextLabel - 1
                           symbolsToLabels = invertMap labelsToSymbols
                           prods = IM.fromList $ prodsToIntProds symbolsToLabels kvs
                        in CFG (IntCFG maxLabel prods) symbolsToLabels labelsToSymbols

productionsCFG :: Ord a => CFG a -> a -> [[a]]
productionsCFG (CFG iGr s2l l2s) symbol =
    case toLabel s2l symbol of
        Nothing  -> []
        Just label -> let prodsInt = productionsInt iGr label
                       in sentencesToSymbol l2s prodsInt

isInCFG :: Ord a => a -> CFG a -> Bool
isInCFG c (CFG iGr s2l _) = case toLabel s2l c of
                                Nothing -> False
                                Just i  -> isInIntCFG i iGr

isNotInCFG :: Ord a => a -> CFG a -> Bool
isNotInCFG c (CFG iGr s2l _) = case toLabel s2l c of
                                   Nothing -> False
                                   Just i  -> isNotInIntCFG i iGr

getLabelsCFG :: Ord a => CFG a -> S.Set a
getLabelsCFG (CFG iGr _ l2s) = let labels = getLabelsInt iGr
                                 in S.map (fromJust . toSymbol l2s) labels

getTerminalsCFG :: Ord a => CFG a -> S.Set a
getTerminalsCFG (CFG iGr _ l2s) = let labels = getTerminalsInt iGr
                                   in S.map (fromJust . toSymbol l2s) labels

getNonTerminalsCFG :: Ord a => CFG a -> S.Set a
getNonTerminalsCFG (CFG iGr _ l2s) = let labels = getNonTerminalsInt iGr
                                      in S.map (fromJust . toSymbol l2s) labels

instance (Ord a, Show a) => Grammar (CFG a) where
    type Repr (CFG a) = a
    productions = productionsCFG
    showLabel _ = show
    isInGrammar = isInCFG
    isNotInGrammar = isNotInCFG
    getLabels = getLabelsCFG
    getTerminals = getTerminalsCFG
    getNonTerminals = getNonTerminalsCFG

instance (Ord a, Show a) => Show (CFG a) where show = showGrammar


{- | A newtype for 'Char'-based context-free grammars. This is solely done to
     improve the pretty-printing representation of the grammar labels.
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
    showLabel _ s = [s]
    isInGrammar s (CharCFG g)= isInCFG s g
    isNotInGrammar s (CharCFG g) = isNotInCFG s g
    getLabels (CharCFG g) = getLabelsCFG g
    getTerminals (CharCFG g) = getTerminalsCFG g
    getNonTerminals (CharCFG g) = getNonTerminalsCFG g

-- | Build a 'CharCFG' from an association list of production rules -- see 'productionsToCFG'.
productionsToCharCFG :: [(Char, [String])] -> CharCFG
productionsToCharCFG = CharCFG . productionsToCFG

instance Show CharCFG where show = showGrammar

{- | A newtype for 'String'-based context-free grammars. This is solely done to
     improve the pretty-printing representation of the grammar labels.
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
    showLabel _ s = s
    isInGrammar s (StringCFG g)= isInCFG s g
    isNotInGrammar s (StringCFG g) = isNotInCFG s g
    getLabels (StringCFG g) = getLabelsCFG g
    getTerminals (StringCFG g) = getTerminalsCFG g
    getNonTerminals (StringCFG g) = getNonTerminalsCFG g

-- | Build a 'StringCFG' from an association list of production rules -- see 'productionsToCFG'.
productionsToStringCFG :: [(String, [[String]])] -> StringCFG
productionsToStringCFG = StringCFG . productionsToCFG

instance Show StringCFG where show = showGrammar
