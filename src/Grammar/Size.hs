{-|
Module      : Grammar.Size
Description : Definition of the Size datatype.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD3
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module exports the 'Size' data type.
-}

module Grammar.Size
( Size
, mean
) where

-- system imports
import Data.Foldable


-- | A type alias for sized random expansion
type Size = Double

mean :: (Foldable t, Fractional a) => t a -> a
mean xs = let meanAcc (tot, n) x = (tot+x, n+1)
              (total, len) = foldl' meanAcc (0, 0::Int) xs
           in total / fromIntegral len
