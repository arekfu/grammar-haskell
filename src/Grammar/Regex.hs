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
( ShowSymbol(..)
, showWord
, QuotingPolicy(..)
, Quoted
, runQuoted
, quote
, quoteString
, escapeChar
, escape
, Regex(..)
, isRegexEmpty
, showRegex
, showRegexUnquoted
, showRegexQuoted
, showRegexChevrons
, needsBracketsWithin
, bracketed
, simplify
, harvest
, reservedChars
) where


-- system imports
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.List (intercalate)
import Data.Foldable (Foldable, foldr)
import qualified Data.Set as S
import Control.Monad.Reader (Reader, runReader, ask)


-- | A class that provides a method that specifies how symbols should be
--   displayed, i.e. converted to strings. Yes, it is similar to 'Show', but I
--   don't like the way 'show' displays quotes in 'Char's and 'String's.
class ShowSymbol a where
    -- | Convert a symbol to a string (see 'show').
    showSymbol :: a -> String

instance ShowSymbol Char where showSymbol c = [c]

-- | Convert a string of symbols to a 'String'
showWord :: ShowSymbol a
         => [a]             -- ^ the string of symbols
         -> String          -- ^ the resulting string representation
showWord = concatMap showSymbol

-- | This datatype defines a quoting policy for symbols.
data QuotingPolicy =
    -- | Stipulate that strings representing symbols must be sandwiched between
    --   'leftQuote' and 'rightQuote'. If 'leftQuote' or 'rightQuote' appear in
    --   the body of the symbol string, they will be escaped.
      Quoting { leftQuote :: Char   -- ^ the left (opening) quote character
              , rightQuote :: Char  -- ^ the right (closing) quote character
              }
    -- | Stipulate that strings representing symbols must not be quoted.
    --   Special characters will be escaped in this case.
    | NoQuoting
    deriving (Eq, Ord, Show, Generic, NFData)

-- | A type synonim to the 'Reader' monad to thread the quoting policy more
-- easily.
type Quoted a = Reader QuotingPolicy a

-- | Run the quoting action.
runQuoted :: Reader QuotingPolicy a
          -> QuotingPolicy
          -> a
runQuoted = runReader

-- | Transform a symbol to a 'String', possibly quoting it and escaping all
--   necessary characters.
quote :: ShowSymbol a
      => a              -- ^ the symbol to represent
      -> Quoted String  -- ^ the resulting representation
quote sym = quoteString $ showSymbol sym

-- | Apply quoting and escaping to an existing string.
quoteString :: String           -- ^ the string to quote
            -> Quoted String    -- ^ the quoted, escaped string
quoteString str = do quoting <- ask
                     case quoting of
                         (Quoting left right) -> return $ left : escape [left, right] str ++ [right]
                         NoQuoting            -> return $ escape reservedChars str

-- | Escape a character, if it is reserved.
escapeChar :: String    -- ^ the list of reserved characters
           -> Char      -- ^ the character to escape
           -> String    -- ^ the resulting escaped character, as a 'String'
escapeChar reserved c | c `elem` reserved = ['\\', c]
                      | otherwise = [c]

-- | Escape all characters in a 'String'
escape :: String    -- ^ the list of reserved characters
       -> String    -- ^ the string to escape
       -> String    -- ^ the escaped string
escape reserved = concatMap (escapeChar reserved)




{- | The Regex datatype. It represents basic regular expressions (no
     backreferences, no repetition counting...).
-}
data Regex a = Empty                    -- ^ The empty regex, matches anything
             | Lit a                    -- ^ A literal
             | QuestionMark (Regex a)   -- ^ Kleene question mark (0 or 1)
             | Plus (Regex a)           -- ^ Kleene plus (1 or more)
             | Star (Regex a)           -- ^ Kleene star (0 or more)
             | Concat [Regex a]         -- ^ Concatenation of regexes
             | Alt [Regex a]            -- ^ Disjunction of regexes
             deriving (Eq, Ord, Generic, NFData, Show)

-- Useful instances
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

-- | Returns 'True' if a 'Regex' is empty.
isRegexEmpty :: Regex a -> Bool
isRegexEmpty Empty = True
isRegexEmpty _ = False

{- | Return 'True' if brackets are required around a regular expression.

   When representing 'Regex'es as 'String's, one sometimes needs to insert
   brackets to enforce the correct order of evaluation. For instance, @'Concat'
   ['Alt' ['Lit' \'a\', 'Lit' \'b\'], 'Lit' \'c\']@ must be represented as
   @"(a|b)c"@, because concatenation binds more strongly than alternation.
   Therefore:

   >>> (Concat []) needsBracketsWithin (Alt [])
   False
   >>> (Alt []) needsBracketsWithin (Concat [])
   True
-}
needsBracketsWithin :: Regex a -- ^ the 
                    -> Regex a
                    -> Bool
(Alt _) `needsBracketsWithin` _ = True
(Concat _) `needsBracketsWithin` (Star _) = True
(Concat _) `needsBracketsWithin` (Plus _) = True
(Concat _) `needsBracketsWithin` (QuestionMark _) = True
(Star _) `needsBracketsWithin` (Star _) = True
(Star _) `needsBracketsWithin` (Plus _) = True
(Star _) `needsBracketsWithin` (QuestionMark _) = True
(Plus _) `needsBracketsWithin` (Star _) = True
(Plus _) `needsBracketsWithin` (Plus _) = True
(Plus _) `needsBracketsWithin` (QuestionMark _) = True
(QuestionMark _) `needsBracketsWithin` (Star _) = True
(QuestionMark _) `needsBracketsWithin` (Plus _) = True
(QuestionMark _) `needsBracketsWithin` (QuestionMark _) = True
_ `needsBracketsWithin` _ = False

-- | Transform a 'Regex' into a 'String' and add brackets if necessary.
bracketed :: ShowSymbol a
          => Regex a        -- ^ the outer 'Regex'
          -> Regex a        -- ^ the inner 'Regex'
          -> Quoted String  -- ^ the 'String' representation
bracketed rout rin = do s <- showRegex rin
                        if rin `needsBracketsWithin` rout
                        then return $ "(" ++ s ++ ")"
                        else return s

-- | Transform a 'Regex' into a 'String' using the given 'QuotingPolicy'.
showRegex :: ShowSymbol a
          => Regex a        -- ^ the 'Regex' to transform
          -> Quoted String  -- ^ the resulting 'String
showRegex Empty = quoteString ""
showRegex (Lit a) = quote a
showRegex r0@(Star r) = (++ "*") <$> bracketed r0 r
showRegex r0@(Plus r) = (++ "+") <$> bracketed r0 r
showRegex r0@(QuestionMark r) = (++ "?") <$> bracketed r0 r
showRegex r0@(Concat rs) = concat <$> mapM (bracketed r0) rs
showRegex r0@(Alt rs) = intercalate "|" <$> mapM (bracketed r0) rs

-- | Transform a 'Regex' into a 'String', quote using double quotes ("like
--   this").
showRegexUnquoted ::ShowSymbol a => Regex a -> String
showRegexUnquoted r = runQuoted (showRegex r) NoQuoting

-- | Transform a 'Regex' into a 'String', quote using double quotes ("like
--   this").
showRegexQuoted ::ShowSymbol a => Regex a -> String
showRegexQuoted r = runQuoted (showRegex r) (Quoting '"' '"')

-- | Transform a 'Regex' into a 'String', quote using angle brackets (<like
--   this>).
showRegexChevrons ::ShowSymbol a => Regex a -> String
showRegexChevrons r = runQuoted (showRegex r) (Quoting '<' '>')

-- | Helper function to remove nested 'Concat' constructors.
spliceConcat :: [Regex a] -> [Regex a]
spliceConcat (Concat rs : rest) = rs ++ spliceConcat rest
spliceConcat (r:rs) = r : spliceConcat rs
spliceConcat [] = []

-- | Helper function to remove nested 'Alt' constructors.
spliceAlt :: [Regex a] -> [Regex a]
spliceAlt (Alt rs : rest) = rs ++ spliceAlt rest
spliceAlt (r:rs) = r : spliceAlt rs
spliceAlt [] = []

{- | Find the first fixed point of iterated applications of a function.
     This is different from the standard library 'fix', which does not take a
     starting value and which finds the _least defined_ fixed point.
-}
fix' :: Eq a => (a -> a) -> a -> a
fix' f x = let x' = f x
            in if x == x' then x else fix' f x'

{- | Simplify a 'Regex'. The simplified 'Regex' describes the same language as
     the original one.
-}
simplify :: Eq a => Regex a -> Regex a
simplify = fix' simplify'

-- | Helper function to implement 'simplify': perform one simplification step.
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

-- | Collect all the symbols that appear in a 'Regex'.
harvest :: Ord a => Regex a -> S.Set a
harvest Empty = S.empty
harvest (Lit a) = S.singleton a
harvest (Concat rs) = foldr (\r set -> harvest r `S.union` set) S.empty rs
harvest (Alt rs) = foldr (\r set -> harvest r `S.union` set) S.empty rs
harvest (Star r) = harvest r
harvest (Plus r) = harvest r
harvest (QuestionMark r) = harvest r

-- | These characters are reserved for use in 'Regex'es and must be escaped if
--   quoting is not used.
reservedChars :: String
reservedChars = "()*+?|\n\r\\"
