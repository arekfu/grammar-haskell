{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Grammar.Internal
( Grammar(..)
, showSentence
, showProductions
, showGrammar
, pick
, productionsToIntCFG
, CFG(..)
, productionsToCFG
, toSym
, toLabel
, unsafeToSym
, unsafeToLabel
, sentenceToSym
, sentencesToSym
, sentenceToLabel
, sentencesToLabel
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

class Grammar g where
    type Repr g :: *
    productions :: g -> Repr g -> [[Repr g]]
    showSymbol :: g -> Repr g -> String
    isInGrammar :: Repr g -> g -> Bool
    isInGrammar x gr = not $ isNotInGrammar x gr
    isNotInGrammar :: Repr g -> g -> Bool
    isNotInGrammar x gr = not $ isInGrammar x gr
    getSymbols :: g -> S.Set (Repr g)
    getTerminals :: g -> S.Set (Repr g)
    getNonTerminals :: g -> S.Set (Repr g)

showSentence :: Grammar g => g -> [Repr g] -> String
showSentence grammar sent = concatMap (showSymbol grammar) sent

showProductions :: Grammar g => g -> Repr g -> [[Repr g]] -> String
showProductions grammar sym ls = let header = showSymbol grammar sym
                                  in concatMap (\l -> header ++ " -> " ++ showSentence grammar l ++ "\n") ls

showGrammar :: Grammar g => g -> String
showGrammar grammar = let symbols = S.toList $ getSymbols grammar
                          prods = map (productions grammar) symbols
                       in concatMap (\(s, p) -> showProductions grammar s p) $ zip symbols prods


pick :: Grammar g => Int -> g -> Repr g -> [Repr g]
pick n grammar sym = productions grammar sym !! n


data IntCFG = IntCFG Int                    -- ^ The next free symbol that can be used in the grammar.
                     (IM.IntMap [[Int]])    -- ^ The production rules.
                     deriving (Eq, Ord)

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



data CFG a = CFG IntCFG (M.Map a Int) (IM.IntMap a)
             deriving (Eq, Ord)

toSym :: Ord a => M.Map a Int -> a -> Maybe Int
toSym dict label = M.lookup label dict

unsafeToSym :: Ord a => M.Map a Int -> a -> Int
unsafeToSym dict = fromJust . (toSym dict)

toLabel :: IM.IntMap a -> Int -> Maybe a
toLabel dict sym = IM.lookup sym dict

unsafeToLabel :: Ord a => IM.IntMap a -> Int -> a
unsafeToLabel dict = fromJust . (toLabel dict)

sentenceToSym :: Ord a => M.Map a Int -> [a] -> [Int]
sentenceToSym dict sentence = concatMap (maybeToList . (toSym dict)) sentence

sentencesToSym :: Ord a => M.Map a Int -> [[a]] -> [[Int]]
sentencesToSym dict sentences = map (sentenceToSym dict) sentences

sentenceToLabel :: IM.IntMap a -> [Int] -> [a]
sentenceToLabel dict sentence = concatMap (maybeToList . (toLabel dict)) sentence

sentencesToLabel :: IM.IntMap a -> [[Int]] -> [[a]]
sentencesToLabel dict sentences = map (sentenceToLabel dict) sentences

prodsToIntProds :: Ord a => M.Map a Int -> [(a, [[a]])] -> [(Int, [[Int]])]
prodsToIntProds dict ((k,vals):prods) = let k' = fromJust $ M.lookup k dict
                                            vals' = map (map (unsafeToSym dict)) vals
                                         in (k', vals'):prodsToIntProds dict prods
prodsToIntProds _ [] = []

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

instance (Show a, Ord a) => Show (CFG a) where show g = showGrammar g

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

productionsToCharCFG :: [(Char, [String])] -> CharCFG
productionsToCharCFG = CharCFG . productionsToCFG

instance Show CharCFG where show g = showGrammar g

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

productionsToStringCFG :: [(String, [[String]])] -> StringCFG
productionsToStringCFG = StringCFG . productionsToCFG

instance Show StringCFG where show g = showGrammar g
