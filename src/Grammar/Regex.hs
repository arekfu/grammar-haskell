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
, Quoting(..)
, showRegex
, showRegexWith
, showRegexQuoted
, showRegexBracketed
, showRegexChar
, showRegexString
, needsBracketsWithin
, bracketed
, simplify
, harvest
) where


-- system imports
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.List (intercalate)
import Data.Foldable (Foldable, foldr)
import qualified Data.Set as S

{- | The Regex datatype.
-}
data Regex a = Empty                    -- ^ The empty regex, matches anything
             | Lit a                    -- ^ A literal
             | QuestionMark (Regex a)   -- ^ Kleene question mark (0 or 1)
             | Plus (Regex a)           -- ^ Kleene plus (1 or more)
             | Star (Regex a)           -- ^ Kleene star (0 or more)
             | Concat [Regex a]         -- ^ Concatenation of regexes
             | Alt [Regex a]            -- ^ Disjunction of regexes
             deriving (Eq, Ord, Generic, NFData, Show)

needsBracketsWithin :: Regex a -> Regex a -> Bool
(Alt _) `needsBracketsWithin` _ = True
(Concat _) `needsBracketsWithin` (Star _) = True
(Concat _) `needsBracketsWithin` (Plus _) = True
(Concat _) `needsBracketsWithin` (QuestionMark _) = True
(Star _) `needsBracketsWithin` (Star _) = True
(Star _) `needsBracketsWithin` (Plus _) = True
(Star _) `needsBracketsWithin` (QuestionMark _) = True
(Star _) `needsBracketsWithin` (Concat _) = True
(Plus _) `needsBracketsWithin` (Star _) = True
(Plus _) `needsBracketsWithin` (Plus _) = True
(Plus _) `needsBracketsWithin` (QuestionMark _) = True
(QuestionMark _) `needsBracketsWithin` (Star _) = True
(QuestionMark _) `needsBracketsWithin` (Plus _) = True
(QuestionMark _) `needsBracketsWithin` (QuestionMark _) = True
_ `needsBracketsWithin` _ = False

data Quoting = Quoting Char Char
             | NoQuoting

quote :: Quoting -> String -> String
quote (Quoting left right) s = left : s ++ [right]
quote NoQuoting s = s

bracketed :: Regex a -> Regex a -> String -> String
bracketed r0 r1 s = if r0 `needsBracketsWithin` r1
                    then "(" ++ s ++ ")"
                    else s

showRegexWith :: Quoting -> (a -> String) -> Regex a -> String
showRegexWith q _ Empty = quote q ""
showRegexWith q s (Lit a) = quote q $ s a
showRegexWith q s r0@(Star r) = bracketed r r0 (showRegexWith q s r) ++ "*"
showRegexWith q s r0@(Plus r) = bracketed r  r0 (showRegexWith q s r) ++ "+"
showRegexWith q s r0@(QuestionMark r) = bracketed r r0 (showRegexWith q s r) ++ "?"
showRegexWith q s r0@(Concat rs) = concatMap (\r -> bracketed r r0 $ showRegexWith q s r) rs
showRegexWith q s r0@(Alt rs) = intercalate "|" $ map (\r -> bracketed r r0 $ showRegexWith q s r) rs

showRegex :: Show a => Regex a -> String
showRegex = showRegexWith NoQuoting show

showRegexQuoted :: Regex Char -> String
showRegexQuoted = showRegexWith (Quoting '"' '"') (:[])

showRegexBracketed :: Regex Char -> String
showRegexBracketed = showRegexWith (Quoting '<' '>') (:[])

showRegexChar :: Regex Char -> String
showRegexChar = showRegexWith NoQuoting (:[])

showRegexString :: Regex String -> String
showRegexString = showRegexWith NoQuoting id

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

fix :: Eq a => (a -> a) -> a -> a
fix f x = let x' = f x
           in if x == x' then x else fix f x'

simplify :: Eq a => Regex a -> Regex a
simplify = fix simplify'

simplify' :: Regex a -> Regex a
simplify' Empty = Empty
simplify' (Lit a) = Lit a
simplify' (Concat [r]) = simplify' r
simplify' (Concat rs) = Concat $ map simplify' $ spliceConcat rs
simplify' (Alt [r]) = simplify' r
simplify' (Alt rs) = Alt $ map simplify' $ spliceAlt rs
simplify' (Star (Star r)) = Star $ simplify' r
simplify' (Star (Plus r)) = Star $ simplify' r
simplify' (Plus (Plus r)) = Plus $ simplify' r
simplify' (Plus (Star r)) = Star $ simplify' r
simplify' (QuestionMark (QuestionMark r)) = QuestionMark $ simplify' r
simplify' (QuestionMark (Star r)) = Star $ simplify' r
simplify' (Star (QuestionMark r)) = Star $ simplify' r
simplify' (QuestionMark (Plus r)) = Star $ simplify' r
simplify' (Plus (QuestionMark r)) = Star $ simplify' r
simplify' (Star r) = Star $ simplify' r
simplify' (Plus r) = Plus $ simplify' r
simplify' (QuestionMark r) = QuestionMark $ simplify' r

harvest :: Ord a => Regex a -> S.Set a
harvest Empty = S.empty
harvest (Lit a) = S.singleton a
harvest (Concat rs) = foldr (\r set -> harvest r `S.union` set) S.empty rs
harvest (Alt rs) = foldr (\r set -> harvest r `S.union` set) S.empty rs
harvest (Star r) = harvest r
harvest (Plus r) = harvest r
harvest (QuestionMark r) = harvest r
