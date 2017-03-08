module Grammar.ParseTree
( ParseTree(..)
, yield
) where

import Data.Tree

newtype ParseTree a = ParseTree (Tree a)

yield :: ParseTree a -> [a]
yield (ParseTree a) = flatten a
