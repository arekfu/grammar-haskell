{-|
Module      : Grammar.Regex
Description : Define the Regex data type.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD3
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module defines the Regex datatype, which is used to construct extended
context-free grammars.
-}

module Grammar.Regex
( Regex(..)
, showRegex
) where


-- system imports
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.List (intercalate)
import Data.Foldable (Foldable)

{- | The Regex datatype.
-}
data Regex a = Empty                    -- ^ The empty regex, matches anything
             | Lit a                    -- ^ A literal
             | Concat [Regex a]         -- ^ Concatenation of regexes
             | Alt [Regex a]            -- ^ Disjunction of regexes
             | Star (Regex a)           -- ^ Kleene star (0 or more)
             | Plus (Regex a)           -- ^ Kleene plus (1 or more)
             | QuestionMark (Regex a)   -- ^ Kleene question mark (0 or 1)
             deriving (Eq, Ord, Generic, NFData)

showRegex :: Show a => Regex a -> String
showRegex Empty = ""
showRegex (Lit a) = show a
showRegex (Concat rs) = concatMap showRegex rs
showRegex (Alt rs) = intercalate " | " $ map showRegex rs
showRegex (Star r) = "(" ++ show r ++ ")*"
showRegex (Plus r) = "(" ++ show r ++ ")+"
showRegex (QuestionMark r) = "(" ++ show r ++ ")?"

instance Show a => Show (Regex a) where show = showRegex

instance Functor Regex where
    fmap _ Empty = Empty
    fmap f (Lit a) = Lit $ f a
    fmap f (Concat rs) = Concat $ map (fmap f) rs
    fmap f (Alt rs) = Alt $ map (fmap f) rs
    fmap f (Star r) = Star $ fmap f r
    fmap f (Plus r) = Plus $ fmap f r
    fmap f (QuestionMark r) = QuestionMark $ fmap f r

instance Foldable Regex where
    foldMap _ Empty = mempty
    foldMap f (Lit a) = f a
    foldMap f (Concat rs) = mconcat $ map (foldMap f) rs
    foldMap f (Alt rs) = mconcat $ map (foldMap f) rs
    foldMap f (Star r) = foldMap f r
    foldMap f (Plus r) = foldMap f r
    foldMap f (QuestionMark r) = foldMap f r
