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
) where


-- system imports
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

{- | The Regex datatype.
-}
data Regex a = Lit a                    -- ^ A literal
             | Concat [Regex a]         -- ^ Concatenation of regexes
             | Alt [Regex a]            -- ^ Disjunction of regexes
             | Star (Regex a)           -- ^ Kleene star (0 or more)
             | Plus (Regex a)           -- ^ Kleene plus (1 or more)
             | QuestionMark (Regex a)   -- ^ Kleene question mark (0 or 1)
             deriving (Eq, Ord, Show, Generic, NFData)
