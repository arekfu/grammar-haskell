{-|
Module      : Grammar.CFG
Description : Datatypes and main functions for CFG grammars.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD3
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the most important datatypes of the package and the
functions that operate on them.
-}

{-# LANGUAGE TypeFamilies #-}

module Grammar.CFG
(
-- * The 'Grammar' typeclass
  Grammar(..)
, allProductions
-- ** Pretty-printing parts of a grammar
-- $examplegrammar
, showWord
, showProductions
, showProductionsBNF
, showGrammarWith
, showGrammar
, showGrammarBNF
-- * Context-free grammars over alphabets of integers
, IntCFG(..)
-- ** Type synonims
, Label
, InverseRelabelling
, Relabelling
-- ** Manipulators
, productionsToIntMap
, intMapToIntCFG
, productionsToIntCFG
, renumberLabel
, renumberLabels
, inverseRenumberLabel
, inverseRenumberLabels
, collectLabels
, renumberMap
-- * Context free grammars over alphabets of arbitrary types
, CFG(..)
, Repr(..)
, productionsToCFG
, gatherAllSymbols
, labelAllSymbols
, toSymbol
, toLabel
, unsafeToSymbol
, unsafeToLabel
, symbolsToLabels
, labelsToSymbols
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
import Data.Maybe (fromJust, mapMaybe)
import Data.Foldable (foldr', toList)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- local imports
import Grammar.Regex

{- |
A typeclass that specifies how grammar datatypes should behave. A production
rule is represented by a list of labels. Alternative production rules
associated with the same label are represented as lists of lists.
-}
class Grammar g where

    -- | The type of the elements of the alphabet described by the grammar @g@.
    data Repr g :: *

    -- | @productions gr sym@ returns the productions associated with label @sym@ in grammar @gr@.
    productions :: g -> Repr g -> Maybe (Regex (Repr g))

    -- | Convert a symbol to a 'String'.
    showSymbol :: Repr g -> String

    -- | Is this symbol part of the grammar?
    isInGrammar :: Repr g -> g -> Bool
    isInGrammar x gr = not $ isNotInGrammar x gr

    -- | Is this symbol part of the grammar?
    isNotInGrammar :: Repr g -> g -> Bool
    isNotInGrammar x gr = not $ isInGrammar x gr

    -- | Is this symbol a terminal?
    isTerminal :: Repr g -> g -> Bool

    -- | Is this symbol a nonterminal?
    isNonTerminal :: Repr g -> g -> Bool

    -- | Returns the set of all symbols used in the grammar
    getSymbols :: g -> S.Set (Repr g)

    -- | Returns the set of all terminals used in the grammar
    getTerminals :: g -> S.Set (Repr g)
    getTerminals g = S.filter (`isTerminal` g) $ getSymbols g

    -- | Returns the set of all nonterminals used in the grammar
    getNonTerminals :: g -> S.Set (Repr g)
    getNonTerminals g = S.filter (`isNonTerminal` g) $ getSymbols g

    -- | Returns the start symbol of the grammar
    startSymbol :: g -> Repr g


-- | Pretty-print a word (a sequence of symbols).
showWord :: Grammar g
         => [Repr g]    -- ^ the word
         -> String      -- ^ the word, pretty-printed as a String
showWord = concatMap showSymbol

{- | Pretty-print all the production rules associated with a symbol.

   >>> putStrLn $ showProductions exampleGrammar 'E'
   E -> E+E
   E -> E*E
   E -> (E)
   E -> I
-}
showProductions :: (Grammar g, Show (Repr g))
                => g            -- ^ the grammar
                -> Repr g       -- ^ the symbol on the left-hand side of the rule
                -> String       -- ^ the pretty-printed production rule
showProductions grammar sym = let header = showSymbol sym
                                  prod = productions grammar sym
                               in case prod of
                                      Nothing   -> ""
                                      Just rule -> header ++ " -> " ++ showRegex rule ++ "\n"

{- | Pretty-print all the production rules associated with a symbol, in
     Backus-Naur form.

   >>> putStrLn $ showProductionsBNF exampleGrammar 'E'
   E := E+E | E*E | (E) | I
-}
showProductionsBNF :: (Grammar g, Show (Repr g))
                   => g            -- ^ the grammar
                   -> Repr g       -- ^ the symbol on the left-hand side of the rule
                   -> String       -- ^ the pretty-printed production rule
showProductionsBNF grammar sym = let header = showSymbol sym
                                     prod = productions grammar sym
                               in case prod of
                                      Nothing   -> ""
                                      Just rule -> header ++ " := " ++ showRegex rule ++ "\n"


{- | Pretty-print all the production rules in a grammar using an external
     function to display lists of production rules.
-}
showGrammarWith :: (Grammar g, Show (Repr g))
                => (g -> Repr g -> String)  -- ^ a function that knows how to
                                            --   display production rules
                                            --   associated with a given symbol
                -> g                        -- ^ the grammar
                -> String                   -- ^ its pretty-printed representation as a String
showGrammarWith showProds grammar = let syms = S.toList $ getNonTerminals grammar
                                        prods = concatMap (showProds grammar) syms
                                        start = "\nStart: " ++ showSymbol (startSymbol grammar)
                                        terms = "\nTerminals: " ++ show (getTerminals grammar)
                                        nonterms = "\nNonterminals: " ++ show (getNonTerminals grammar)
                                     in prods ++ start ++ terms ++ nonterms


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
showGrammar :: (Grammar g, Show (Repr g))
            => g                        -- ^ the grammar
            -> String                   -- ^ its pretty-printed representation as a String
showGrammar = showGrammarWith showProductions


{- | Pretty-print all the production rules in a grammar, in Backus-Naur form.

   >>> putStrLn $ showGrammarBNF exampleGrammar
   E := E+E | E*E | (E) | I
   I := a | b | c | Ia | Ib | Ic | I0 | I1 | I2 | I3
-}
showGrammarBNF :: (Grammar g, Show (Repr g))
               => g                        -- ^ the grammar
               -> String                   -- ^ its pretty-printed representation as a String
showGrammarBNF = showGrammarWith showProductionsBNF




------------------------------------
--  generic grammar manipulators  --
------------------------------------


-- | Represent all grammar production rules as an association list
allProductions :: Grammar g => g -> [(Repr g, Regex (Repr g))]
allProductions g = let nonterms = toList $ getNonTerminals g
                    in zip nonterms $ mapMaybe (productions g) nonterms


---------------------------------------
--  Int-based grammar type (IntCFG)  --
---------------------------------------

{- |
The IntCFG datatype implements a context-free grammar as a set of production
rules between strings of integers. The datatype requires the integers appearing
in the grammar to be consecutive, starting at @0@, which is taken to be the
start symbol of the grammar. This invariant is enforced on construction (see
'productionsToIntCFG'). The production rules are internally represented as an
'Data.IntMap.IntMap' of '[Data.Vector.Unboxed.Vector Int]'.

Furthermore, the datatype assumes without loss of generality that nonterminal
symbols take up labels from @0@ to some index @i@ (inclusive); terminals take
up the remaining labels, from @i+1@ to @n-1@ (again, inclusive). The datatype
remembers the index @i@ and uses it to provide a quick answer to question such
as "is this symbol terminal?".

For efficiency reasons, the 'CFG' datatype is built upon an 'IntCFG'.
-}

data IntCFG = IntCFG Label Label (IM.IntMap (Regex Label)) deriving (Eq, Ord, Generic, NFData)
-- ^ The default constructor takes the next available label @n@, the label of
-- the first nonterminal symbol and the 'Data.IntMap.IntMap' representing the
-- production rules.

-- | Type synonim for 'Int'.
type Label = Int

productionsInt :: IntCFG -> Label -> Maybe (Regex Label)
productionsInt (IntCFG _ _ prods) nt = IM.lookup nt prods

isInIntCFG :: Label -> IntCFG -> Bool
isInIntCFG c (IntCFG n _ _) = c >= 0 && c < n

isNotInIntCFG :: Label -> IntCFG -> Bool
isNotInIntCFG c (IntCFG n _ _) = c < 0 || c >= n

isTerminalInt :: Label -> IntCFG -> Bool
isTerminalInt c (IntCFG n i _) = c >= i && c < n

isNonTerminalInt :: Label -> IntCFG -> Bool
isNonTerminalInt c (IntCFG _ i _) = c >= 0 && c < i

getSymbolsInt :: IntCFG -> S.Set Label
getSymbolsInt (IntCFG n _ _) = S.fromDistinctAscList [0..n-1]

getTerminalsInt :: IntCFG -> S.Set Label
getTerminalsInt (IntCFG n i _) =  S.fromDistinctAscList [i..n-1]

getNonTerminalsInt :: IntCFG -> S.Set Label
getNonTerminalsInt (IntCFG _ i _) =  S.fromDistinctAscList [0..i-1]

-- | Type synonim for a 'Data.Vector.Unboxed.Vector' of 'Int's.
type InverseRelabelling = VU.Vector Int

-- | Type synonim for a 'Data.IntMap.IntMap' of 'Int's.
type Relabelling = IM.IntMap Int

-- | Invert an 'Int'-to-'Int' relabelling.
invert :: InverseRelabelling -> Relabelling
invert = VU.ifoldr' (\i label inverse -> IM.insert label i inverse) IM.empty

-- | Apply the inverse relabelling to a given 'Label'.
inverseRenumberLabel :: InverseRelabelling -> Label -> Label
inverseRenumberLabel renumb label = renumb VU.! label

-- | Apply the inverse relabelling to a given 'Regex' 'Label'
inverseRenumberLabels :: InverseRelabelling -> Regex Label -> Regex Label
inverseRenumberLabels renumb = fmap (inverseRenumberLabel renumb)

-- | Apply the relabelling to a given 'Label'
renumberLabel :: Relabelling -> Label -> Label
renumberLabel iRenumb label = fromJust $ IM.lookup label iRenumb

-- | Apply the relabelling to a given 'Regex' 'Label'
renumberLabels :: Relabelling -> Regex Label -> Regex Label
renumberLabels iRenumb = fmap (renumberLabel iRenumb)

-- | Return all labels appearing in a set of (integer) production rules.
collectLabels :: IM.IntMap (Regex Label) -> IS.IntSet
collectLabels =
    let insertStrings :: IS.IntSet -> Regex Label -> IS.IntSet
        insertStrings = foldr' IS.insert
     in IM.foldrWithKey (\key values set -> insertStrings (IS.insert key set) values) IS.empty

{- | Build an 'IntCFG' from an association list of @(nonterminal, productions)@
     pairs. The Labels appearing in the association list are renumbered to
     enforce the invariant that they span the @[0,n-1]@ range. The relabelling
     is returned along with the built grammar, in the form of a
     'Data.Vector.Unboxed.Vector' 'Label' indexed by the renumbered labels.
-}
productionsToIntCFG :: Label -> [(Label, Regex Label)] -> (IntCFG, InverseRelabelling, Relabelling)
productionsToIntCFG start = intMapToIntCFG start . productionsToIntMap

{- | Build an 'IntCFG' from an 'Data.IntMap.IntMap' between 'Label's. The
     'Labels' will be renumbered as specified in the second and third return
     values.
-}
intMapToIntCFG :: Label -> IM.IntMap (Regex Label) -> (IntCFG, InverseRelabelling, Relabelling)
intMapToIntCFG start intMap = let (intMap', inverseRelabelling, relabelling) = renumberMap start intMap
                                  maxLabel = VU.length inverseRelabelling - 1
                                  nNonTerms = IM.size intMap
                               in (IntCFG maxLabel nNonTerms intMap', inverseRelabelling, relabelling)

{- | Renumber the @n@ integers that appear as keys and values of the map in
     such a way that they exactly span the @[0,n-1]@ interval. The
     correspondence between the new and the old numbering scheme
     ('InverseRelabelling') is returned as a 'Data.Vector.Unboxed.Vector'.
     The mapping in the other direction ('SymbolToLabelDict') is actually an
     'Data.IntMap.IntMap'.  The starting symbol is always renumbered as 0.
-}
renumberMap :: Label                    -- ^ The label of the starting symbol
            -> IM.IntMap (Regex Label)  -- ^ The map to renumber
            -> (IM.IntMap (Regex Label), InverseRelabelling, Relabelling)
            -- ^ The renumbered map, the new-to-old mapping and the old-to-new mapping
renumberMap start intMap =
    let allLabels = collectLabels intMap
        nonTerms = IM.keysSet intMap
        nonTermsExceptStart = IS.delete start nonTerms
        terms = allLabels `IS.difference` nonTerms
        inverseRelabelling = VU.fromList $ start : IS.toList nonTermsExceptStart ++ IS.toList terms
        relabelling = invert inverseRelabelling
        intMap' = IM.mapKeys (renumberLabel relabelling) intMap
        intMap'' = fmap (renumberLabels relabelling) intMap'
     in (intMap'', inverseRelabelling, relabelling)

{- | Provide the fundamental building block to construct an 'IntCFG' from an
     association list of @(nonterminal, productions)@ pairs. The Labels are
     not guaranteed to span the @[0,n-1]@ range.
-}
productionsToIntMap :: [(Label, Regex Label)] -> IM.IntMap (Regex Label)
productionsToIntMap = IM.fromList

instance Grammar IntCFG where
    data Repr IntCFG = ReprInt { unReprInt :: Label } deriving (Eq, Ord, Show)
    -- all the instance declarations do is actually wrap and unwrap the Repr
    -- datatype
    productions g (ReprInt sym) = (fmap ReprInt) <$> productionsInt g sym
    showSymbol (ReprInt sym) = show sym
    isInGrammar (ReprInt sym) = isInIntCFG sym
    isNotInGrammar (ReprInt sym) = isNotInIntCFG sym
    isTerminal (ReprInt sym) = isTerminalInt sym
    isNonTerminal (ReprInt sym) = isNonTerminalInt sym
    getSymbols = S.map ReprInt . getSymbolsInt
    getTerminals = S.map ReprInt . getTerminalsInt
    getNonTerminals = S.map ReprInt . getNonTerminalsInt
    startSymbol _ = ReprInt 0

instance Show IntCFG where show = showGrammarBNF


{- | A datatype representing context-free grammars over alphabets of arbitrary
type. The implementation uses an 'IntCFG' grammar to represent the relations
between the labels.

Conversion from __symbols__ (i.e. objects of the external representation type,
@a@) to __labels__ (i.e. 'Int's, the internal representation type) are
represented by a 'Data.Map.Map'; this implies that, for the most part, the type
variable @a@ is required to be in class 'Ord', although this constraint is not
enforced at the level of the datatype.

Conversion in the opposite direction (from __labels__ to __symbols__) are
represented by a 'Data.Vector.Vector', because labels are guaranteed to span a
continuous range starting at @0@ (see 'IntCFG').
-}
data CFG a = CFG a IntCFG (SymbolToLabelDict a) (LabelToSymbolDict a)
             deriving (Eq, Ord, Generic, NFData)

type LabelToSymbolDict a = V.Vector a
type SymbolToLabelDict a = M.Map a Label

-- | Convert a symbol to a label, given a dictionary.
toLabel :: Ord a
        => SymbolToLabelDict a  -- ^ the symbol-to-label dictionary
        -> a            -- ^ the symbol to convert
        -> Maybe Label  -- ^ the resulting label, maybe
toLabel dict symbol = M.lookup symbol dict

-- | Like 'toLabel', but assume the symbol is in the dictionary.
unsafeToLabel :: Ord a => SymbolToLabelDict a -> a -> Label
unsafeToLabel dict = fromJust . toLabel dict

-- | Convert a label to a symbol, given a dictionary.
toSymbol :: LabelToSymbolDict a  -- ^ the label-to-symbol dictionary
         -> Label               -- ^ the label to convert
         -> Maybe a             -- ^ the resulting symbol, maybe
toSymbol dict label = dict V.!? label

-- | Like 'toSymbol', but assume the label is in the dictionary.
unsafeToSymbol :: LabelToSymbolDict a -> Label -> a
unsafeToSymbol dict label =  dict V.! label

-- | Convert a 'Regex' of symbols to a 'Regex' of 'Label's.
symbolsToLabels :: (Functor f, Ord a) => SymbolToLabelDict a -> f a -> f Label
symbolsToLabels dict = fmap (unsafeToLabel dict)

-- | Convert a 'Functor' of 'Label's to a 'Functor' of symbols.
labelsToSymbols :: Functor f => LabelToSymbolDict a -> f Label -> f a
labelsToSymbols dict = fmap (unsafeToSymbol dict)

-- | Extract all the symbols from an association list of production rules.
gatherAllSymbols :: Ord a => [(a, [[a]])] -> S.Set a
gatherAllSymbols = foldr' insertKeyValue S.empty
    where insertKeyValue :: Ord a => (a, [[a]]) -> S.Set a -> S.Set a
          insertKeyValue (k, vs) set = foldr' S.insert (S.insert k set) $ concat vs

{- | Given a list of production rules in the form of an association list,
     associate a label to each mentioned symbol and return an association list
     of labels, along with dictionaries to translate symbols to labels and
     vice-versa.
-}
labelAllSymbols :: Ord a => [(a, [[a]])] -> ([(Label, Regex Label)], LabelToSymbolDict a, SymbolToLabelDict a)
labelAllSymbols kvs = let allSymbols = gatherAllSymbols kvs
                          labelsToSymbolsDict = V.fromList $ S.toList allSymbols
                          symbolsToLabelsDict = invertSymbolToLabelDict labelsToSymbolsDict
                          labelledAssocList = labelKeyValues kvs symbolsToLabelsDict
                       in (labelledAssocList, labelsToSymbolsDict, symbolsToLabelsDict)

labelKeyValues :: Ord a => [(a, [[a]])] -> SymbolToLabelDict a -> [(Label, Regex Label)]
labelKeyValues kvs dict =
    let translate s = fromJust $ M.lookup s dict
     in map (\(k, vs) -> (translate k, Alt (map (Concat . map (Lit . translate)) vs))) kvs

-- | Invert an 'Int'-to-@a@ labelling.
invertSymbolToLabelDict :: Ord a => LabelToSymbolDict a -> SymbolToLabelDict a
invertSymbolToLabelDict = V.ifoldr' (\i sym inverse -> M.insert sym i inverse) M.empty

relabel :: LabelToSymbolDict a -> InverseRelabelling -> LabelToSymbolDict a
relabel labelsToSymbolsDict inverseRelabelling =
    V.generate (V.length labelsToSymbolsDict) (\i -> labelsToSymbolsDict V.! (inverseRelabelling VU.! i))

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
   > exampleGrammar :: CFG Char
   > exampleGrammar = productionsToCFG 'E' exampleKeyValue
-}
productionsToCFG :: (Ord a, Show a) => a -> [(a, [[a]])] -> CFG a
productionsToCFG start kvs =
    let (ikvs, labelsToSymbolsDict, symbolsToLabelsDict) = labelAllSymbols kvs
        startLabel = fromJust $ M.lookup start symbolsToLabelsDict
        (intCFG, inverseRelabelling, _) = productionsToIntCFG startLabel ikvs
        labelsToSymbolsDict' = relabel labelsToSymbolsDict inverseRelabelling
        symbolsToLabelsDict' = invertSymbolToLabelDict labelsToSymbolsDict'
     in CFG start intCFG symbolsToLabelsDict' labelsToSymbolsDict'

productionsCFG :: Ord a => CFG a -> a -> Maybe (Regex a)
productionsCFG (CFG _ iGr s2l l2s) symbol =
    do label <- toLabel s2l symbol
       prodsInt <- productionsInt iGr label
       return $ labelsToSymbols l2s prodsInt

isInCFG :: Ord a => a -> CFG a -> Bool
isInCFG c (CFG _ iGr s2l _) = case toLabel s2l c of
                                Nothing -> False
                                Just i  -> isInIntCFG i iGr

isNotInCFG :: Ord a => a -> CFG a -> Bool
isNotInCFG c (CFG _ iGr s2l _) = case toLabel s2l c of
                                   Nothing -> False
                                   Just i  -> isNotInIntCFG i iGr

isTerminalCFG :: Ord a => a -> CFG a -> Bool
isTerminalCFG s (CFG _ iGr s2l _) =
    case toLabel s2l s of
        Nothing -> False
        Just l  -> isTerminalInt l iGr

isNonTerminalCFG :: Ord a => a -> CFG a -> Bool
isNonTerminalCFG s (CFG _ iGr s2l _) =
    case toLabel s2l s of
        Nothing -> False
        Just l  -> isNonTerminalInt l iGr

getSymbolsCFG :: Ord a => CFG a -> S.Set a
getSymbolsCFG (CFG _ iGr _ l2s) = let labels = getSymbolsInt iGr
                                   in S.map (unsafeToSymbol l2s) labels

getTerminalsCFG :: Ord a => CFG a -> S.Set a
getTerminalsCFG (CFG _ iGr _ l2s) = let labels = getTerminalsInt iGr
                                     in S.map (unsafeToSymbol l2s) labels

getNonTerminalsCFG :: Ord a => CFG a -> S.Set a
getNonTerminalsCFG (CFG _ iGr _ l2s) = let labels = getNonTerminalsInt iGr
                                        in S.map (unsafeToSymbol l2s) labels

startSymbolCFG :: CFG a -> a
startSymbolCFG (CFG start _ _ _) = start

instance (Eq a, Ord a, Show a) => Grammar (CFG a) where
    data Repr (CFG a) = ReprCFG { unReprCFG :: a } deriving (Eq, Ord, Show)
    productions grammar (ReprCFG s) = (fmap ReprCFG) <$> productionsCFG grammar s
    showSymbol = show
    isInGrammar (ReprCFG s) = isInCFG s
    isNotInGrammar (ReprCFG s) = isNotInCFG s
    isTerminal (ReprCFG s) = isTerminalCFG s
    isNonTerminal (ReprCFG s) = isNonTerminalCFG s
    getSymbols = S.map ReprCFG . getSymbolsCFG
    getTerminals = S.map ReprCFG . getTerminalsCFG
    getNonTerminals = S.map ReprCFG . getNonTerminalsCFG
    startSymbol = ReprCFG . startSymbolCFG

instance (Ord a, Show a) => Show (CFG a) where show = showGrammarBNF


{- | A newtype for 'Char'-based context-free grammars. This is solely done to
     improve the pretty-printing representation of the grammar labels.
     Compare:

     >>> putStrLn $ showGrammar (productionsToCFG [('A', ["a"])])
     'A' := 'a'
     >>> putStrLn $ showGrammar (productionsToCharCFG [('A', ["a"])])
     A := a
-}
newtype CharCFG = CharCFG (CFG Char) deriving (Eq, Ord, Generic, NFData)

instance Grammar CharCFG where
    data Repr CharCFG = ReprChar { unReprChar :: Char } deriving (Eq, Ord, Show)
    productions (CharCFG g) (ReprChar c) = (fmap ReprChar) <$> productionsCFG g c
    showSymbol (ReprChar s) = [s]
    isInGrammar (ReprChar s) (CharCFG g) = isInCFG s g
    isNotInGrammar (ReprChar s) (CharCFG g) = isNotInCFG s g
    isTerminal (ReprChar s) (CharCFG g) = isTerminalCFG s g
    isNonTerminal (ReprChar s) (CharCFG g) = isNonTerminalCFG s g
    getSymbols (CharCFG g) = S.map ReprChar $ getSymbolsCFG g
    getTerminals (CharCFG g) = S.map ReprChar $ getTerminalsCFG g
    getNonTerminals (CharCFG g) = S.map ReprChar $ getNonTerminalsCFG g
    startSymbol (CharCFG g) = ReprChar $ startSymbolCFG g

-- | Build a 'CharCFG' from an association list of production rules -- see 'productionsToCFG'.
productionsToCharCFG :: Char -> [(Char, [String])] -> CharCFG
productionsToCharCFG start = CharCFG . productionsToCFG start

instance Show CharCFG where show = showGrammarBNF

{- | A newtype for 'String'-based context-free grammars. This is solely done to
     improve the pretty-printing representation of the grammar labels.
     Compare:

     >>> putStrLn $ showGrammar (productionsToCFG [("NONTERM", [["term"]])])
     "NONTERM" := "term"
     >>> putStrLn $ showGrammar (productionsToStringCFG [("NONTERM", [["term"]])])
     NONTERM := term
-}
newtype StringCFG = StringCFG (CFG String) deriving (Eq, Ord, Generic, NFData)

instance Grammar StringCFG where
    data Repr StringCFG = ReprString { unReprString :: String } deriving (Eq, Ord, Show)
    productions (StringCFG g) (ReprString s) = (fmap ReprString) <$> productionsCFG g s
    showSymbol (ReprString s) = s
    isInGrammar (ReprString s) (StringCFG g)= isInCFG s g
    isNotInGrammar (ReprString s) (StringCFG g) = isNotInCFG s g
    isTerminal (ReprString s) (StringCFG g) = isTerminalCFG s g
    isNonTerminal (ReprString s) (StringCFG g) = isNonTerminalCFG s g
    getSymbols (StringCFG g) = S.map ReprString $ getSymbolsCFG g
    getTerminals (StringCFG g) = S.map ReprString $ getTerminalsCFG g
    getNonTerminals (StringCFG g) = S.map ReprString $ getNonTerminalsCFG g
    startSymbol (StringCFG g) = ReprString $ startSymbolCFG g

-- | Build a 'StringCFG' from an association list of production rules -- see 'productionsToCFG'.
productionsToStringCFG :: String -> [(String, [[String]])] -> StringCFG
productionsToStringCFG start = StringCFG . productionsToCFG start

instance Show StringCFG where show = showGrammarBNF
