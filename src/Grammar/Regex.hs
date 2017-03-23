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
, showRegexWith
, showRegexChar
, showRegexString
, simplify
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
             deriving (Eq, Ord, Generic, NFData, Show)

showRegexWith :: (a -> String) -> Regex a -> String
showRegexWith _ Empty = ""
showRegexWith s (Lit a) = s a
showRegexWith s (Concat rs) = "(" ++ concatMap (showRegexWith s) rs ++ ")"
showRegexWith s (Alt rs) = "(" ++ intercalate "|" (map (showRegexWith s) rs) ++ ")"
showRegexWith s (Star r) = "(" ++ showRegexWith s r ++ ")*"
showRegexWith s (Plus r) = "(" ++ showRegexWith s r ++ ")+"
showRegexWith s (QuestionMark r) = "(" ++ showRegexWith s r ++ ")?"

showRegex :: Show a => Regex a -> String
showRegex = showRegexWith show

showRegexChar :: Regex Char -> String
showRegexChar = showRegexWith (:[])

showRegexString :: Regex String -> String
showRegexString = showRegexWith id

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

spliceConcat :: [Regex a] -> [Regex a]
spliceConcat (Concat rs : rest) = rs ++ spliceConcat rest
spliceConcat (r:rs) = r : spliceConcat rs
spliceConcat [] = []

spliceAlt :: [Regex a] -> [Regex a]
spliceAlt (Alt rs : rest) = rs ++ spliceAlt rest
spliceAlt (r:rs) = r : spliceAlt rs
spliceAlt [] = []

simplify :: Regex a -> Regex a
simplify Empty = Empty
simplify (Lit a) = Lit a
simplify (Concat [r]) = simplify r
simplify (Concat rs) = Concat $ map simplify $ spliceConcat rs
simplify (Alt [r]) = simplify r
simplify (Alt rs) = Alt $ map simplify $ spliceAlt rs
simplify (Star (Star r)) = Star $ simplify r
simplify (Star (Plus r)) = Star $ simplify r
simplify (Plus (Plus r)) = Plus $ simplify r
simplify (Plus (Star r)) = Star $ simplify r
simplify (QuestionMark (QuestionMark r)) = QuestionMark $ simplify r
simplify (QuestionMark (Star r)) = Star $ simplify r
simplify (Star (QuestionMark r)) = Star $ simplify r
simplify (QuestionMark (Plus r)) = Star $ simplify r
simplify (Plus (QuestionMark r)) = Star $ simplify r
simplify (Star r) = Star $ simplify r
simplify (Plus r) = Plus $ simplify r
simplify (QuestionMark r) = QuestionMark $ simplify r
