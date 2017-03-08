module Grammar
( Symbol(..)
, Sentential(..)
, CSG(..)
, apply
, pick
, concatSent
, productionsToGrammar
, nonterminals
, allSymbols
, alphabet
) where

-- system imports
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (maybeToList)

newtype Symbol a = S a deriving (Eq, Ord)
instance Show a => Show (Symbol a) where
    show (S x) = show x
instance Functor Symbol where
    fmap f (S sym) = S $ f sym

newtype Sentential a = Sentential { getSyms :: [Symbol a] } deriving (Eq, Ord)

instance Show a => Show (Sentential a) where
    show (Sentential ls) = concatMap show ls
instance Functor Sentential where
    fmap f (Sentential sent) = Sentential $ map (fmap f) sent

concatSent :: [Sentential a] -> Sentential a
concatSent = Sentential . concatMap getSyms

newtype CSG a = CSG (M.Map (Symbol a) [Sentential a]) deriving (Eq, Ord)

showProduction :: Show a => Symbol a -> [Sentential a] -> String
showProduction sym ls = let header = show sym
                         in concatMap (\l -> header ++ " -> " ++ show l ++ "\n") ls

instance Show a => Show (CSG a) where
    show (CSG prods) = concat $ M.mapWithKey showProduction prods

keyValueToAssocList :: Ord a => (a, [[a]]) -> (Symbol a, [Sentential a])
keyValueToAssocList (k, v) = (S k, map (Sentential . map S) v)

productionsToGrammar :: Ord a => [(a, [[a]])] -> CSG a
productionsToGrammar kvs = let assocList = map keyValueToAssocList kvs
                            in CSG $ M.fromList assocList

apply :: Ord a => CSG a -> Symbol a -> [Sentential a]
apply (CSG p) nt = let inMap = M.lookup nt p
                  in concat $ maybeToList inMap

pick :: Ord a => Int -> CSG a -> Symbol a -> Sentential a
pick n ps sym = apply ps sym !! n

nonterminals :: Ord a => CSG a -> S.Set (Symbol a)
nonterminals (CSG prods) = S.fromList $ M.keys prods

allSymbols :: Ord a => CSG a -> S.Set (Symbol a)
allSymbols (CSG prods) = S.fromList $ getSyms $ concatSent $ concat $ M.elems prods

alphabet :: Ord a => CSG a -> S.Set (Symbol a)
alphabet g = let allSyms = allSymbols g
                 nonterms = nonterminals g
              in allSyms `S.difference` nonterms
