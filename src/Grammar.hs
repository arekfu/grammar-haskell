module Grammar
( Symbol(..)
, Sentention(..)
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

newtype Sentention a = Sentention { getSyms :: [Symbol a] } deriving (Eq, Ord)

instance Show a => Show (Sentention a) where
    show (Sentention ls) = concatMap show ls
instance Functor Sentention where
    fmap f (Sentention sent) = Sentention $ map (fmap f) sent

concatSent :: [Sentention a] -> Sentention a
concatSent = Sentention . concatMap getSyms

newtype CSG a = CSG (M.Map (Symbol a) [Sentention a]) deriving (Eq, Ord)

showProduction :: Show a => Symbol a -> [Sentention a] -> String
showProduction sym ls = let header = show sym
                         in concatMap (\l -> header ++ " -> " ++ show l ++ "\n") ls

instance Show a => Show (CSG a) where
    show (CSG prods) = concat $ M.mapWithKey showProduction prods

keyValueToAssocList :: Ord a => (a, [[a]]) -> (Symbol a, [Sentention a])
keyValueToAssocList (k, v) = (S k, map (Sentention . map S) v)

productionsToGrammar :: Ord a => [(a, [[a]])] -> CSG a
productionsToGrammar kvs = let assocList = map keyValueToAssocList kvs
                            in CSG $ M.fromList assocList

apply :: Ord a => CSG a -> Symbol a -> [Sentention a]
apply (CSG p) nt = let inMap = M.lookup nt p
                  in concat $ maybeToList inMap

pick :: Ord a => Int -> CSG a -> Symbol a -> Sentention a
pick n ps sym = apply ps sym !! n

nonterminals :: Ord a => CSG a -> S.Set (Symbol a)
nonterminals (CSG prods) = S.fromList $ M.keys prods

allSymbols :: Ord a => CSG a -> S.Set (Symbol a)
allSymbols (CSG prods) = S.fromList $ getSyms $ concatSent $ concat $ M.elems prods

alphabet :: Ord a => CSG a -> S.Set (Symbol a)
alphabet g = let allSyms = allSymbols g
                 nonterms = nonterminals g
              in allSyms `S.difference` nonterms
