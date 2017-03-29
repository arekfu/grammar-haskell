{-|
Module      : Grammar.CFG.Random
Description : Functions to randomly expand symbols and words according to a grammar.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD3
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the relevant tools to sample random words from a
context-free grammar. All computations take place in the 'MC' monad, which is
simply a practical way to thread the pseudo-random-number generator (PRNG)
state. Be careful: the expansion is currently totally unbiased -- it will
expand a given nonterminal by assuming that all of the production rules have
equal probability. Depending on the grammar, this may lead the size of the
generated word to grow without bound, or to become very large.
-}

module Grammar.CFG.Random
(
-- * Randomly expanding symbols and words
  RandomGrammar(..)
, randomSymExpand
, randomWordExpand
, randomGrammarDerive
, randomGrammarDeriveN
, randomGrammarDeriveScan
) where

-- system imports
import Prelude hiding (words)

-- local imports
import Grammar.CFG
import Grammar.MC
import Grammar.Regex.Random (randomExpandRegex)

---------------------------------------------------------
--  functions to randomly derive sequences of symbols  --
---------------------------------------------------------

{- | A typeclass for random generation of language strings starting from a
     grammar specification, in the form of a type of the 'Grammar.CFG.Grammar'
     class.
-}
class (Grammar g, Ord (Repr g)) => RandomGrammar g where

    -- | Recursively and randomly expand a word until it consists solely of
    --   terminals. WARNING: may produce infinite lists!
    randomWordDerive :: g           -- ^ the grammar
                     -> [Repr g]    -- ^ the word to expand
                     -> MC [Repr g] -- ^ a fully expanded sequence of terminals
    randomWordDerive grammar word =
        do expanded <- randomWordExpand grammar word
           if word == expanded
           then return expanded
           else randomWordDerive grammar expanded

    -- | Recursively and randomly expand a word until it consists solely of
    --   terminals or until @n@ expansion steps have been performed, whichever
    --   comes first.
    randomWordDeriveN :: Int            -- ^ the maximum number of expansions
                      -> g              -- ^ the grammar
                      -> [Repr g]       -- ^ the starting word
                      -> MC [Repr g]    -- ^ the resulting expansion
    randomWordDeriveN 0 _ word = return word
    randomWordDeriveN n grammar word = do expanded <- randomWordDerive grammar word
                                          randomWordDeriveN (n-1) grammar expanded

    -- | Recursively and randomly expand a word, and return all the
    --   intermediate expansion results. WARNING: may produce infinite lists!
    randomWordDeriveScan :: g               -- ^ the grammar
                         -> [Repr g]        -- ^ the starting word
                         -> MC [[Repr g]]   -- ^ the list of all intermediate expansions
    randomWordDeriveScan grammar word =
        do expanded <- randomWordExpand grammar word
           if word == expanded
           then return [expanded]
           else fmap (expanded:) (randomWordDeriveScan grammar expanded)


-- | Randomly expand a symbol using one of its production rules.
randomSymExpand :: (Grammar g, Ord (Repr g))
                => g            -- ^ the grammar
                -> Repr g       -- ^ the symbol to expand
                -> MC [Repr g]  -- ^ the resulting word
randomSymExpand gr sym = case productions gr sym of
                                Nothing    -> return [sym]
                                Just regex -> randomExpandRegex regex

-- | Expand a word (a sequence of symbols) using randomly selected
--   production rules for each nonterminal.
randomWordExpand :: (Grammar g, Ord (Repr g))
                 => g           -- ^ the grammar
                 -> [Repr g]    -- ^ the word to expand
                 -> MC [Repr g] -- ^ the resulting word
randomWordExpand g syms = do words <- mapM (randomSymExpand g) syms
                             return $ concat words

-- | Recursively and randomly expand the start symbol of a grammar until it
--   consists solely of terminals. WARNING: may produce infinite lists!
randomGrammarDerive :: (RandomGrammar g, Ord (Repr g))
                    => g           -- ^ the grammar
                    -> MC [Repr g] -- ^ a fully expanded sequence of terminals
randomGrammarDerive grammar = randomWordDerive grammar [startSymbol grammar]

-- | Recursively and randomly expand the start symbol of a grammar until it
--   consists solely of terminals or until @n@ expansion steps have been
--   performed, whichever comes first.
randomGrammarDeriveN :: (RandomGrammar g, Ord (Repr g))
                     => Int            -- ^ the maximum number of expansions
                     -> g              -- ^ the grammar
                     -> MC [Repr g]    -- ^ the resulting expansion
randomGrammarDeriveN n grammar = randomWordDeriveN n grammar [startSymbol grammar]

-- | Recursively and randomly expand the start symbol of a grammar, and return
--   all the intermediate expansion results. WARNING: may produce infinite
--   lists!
randomGrammarDeriveScan :: (RandomGrammar g, Ord (Repr g))
                        => g               -- ^ the grammar
                        -> MC [[Repr g]]   -- ^ the list of all intermediate expansions
randomGrammarDeriveScan grammar = randomWordDeriveScan grammar [startSymbol grammar]

delegateCFGToIntCFG :: (Functor f, Ord b)
                    => (forall g. RandomGrammar g => g -> f (Repr g) -> MC (f (Repr g)))
                    -> CFG b
                    -> f (Repr (CFG b))
                    -> MC (f (Repr (CFG b)))
delegateCFGToIntCFG action (CFG _ iGr s2l l2s) word =
    let labelWord = ReprInt <$> symbolsToLabels s2l (unReprCFG <$> word)
     in do derived <- action iGr labelWord
           return (ReprCFG <$> labelsToSymbols l2s (unReprInt <$> derived))

delegateCFGToIntCFG2 :: (Functor f, Ord b)
                     => (forall g. RandomGrammar g => g -> f (Repr g) -> MC (f (f (Repr g))))
                     -> CFG b
                     -> f (Repr (CFG b))
                     -> MC (f (f (Repr (CFG b))))
delegateCFGToIntCFG2 action (CFG _ iGr s2l l2s) regex =
    let labelRegex = ReprInt <$> symbolsToLabels s2l (unReprCFG <$> regex)
     in do derived <- action iGr labelRegex
           return (fmap ReprCFG <$> fmap (labelsToSymbols l2s) (fmap unReprInt <$> derived))

instance RandomGrammar IntCFG

instance (Ord a, Show a) => RandomGrammar (CFG a) where
    randomWordDerive = delegateCFGToIntCFG randomWordDerive
    randomWordDeriveN n = delegateCFGToIntCFG (randomWordDeriveN n)
    randomWordDeriveScan = delegateCFGToIntCFG2 randomWordDeriveScan

instance RandomGrammar CharCFG where
    randomWordDerive (CharCFG g) word =
        map (ReprChar . unReprCFG)
            <$> delegateCFGToIntCFG randomWordDerive g (map (ReprCFG . unReprChar) word)
    randomWordDeriveN n (CharCFG g) word =
        map (ReprChar . unReprCFG)
            <$> delegateCFGToIntCFG (randomWordDeriveN n) g (map (ReprCFG . unReprChar) word)
    randomWordDeriveScan (CharCFG g) word =
        map (map (ReprChar . unReprCFG))
            <$> delegateCFGToIntCFG2 randomWordDeriveScan g (map (ReprCFG . unReprChar) word)

instance RandomGrammar StringCFG where
    randomWordDerive (StringCFG g) word =
        map (ReprString . unReprCFG)
            <$> delegateCFGToIntCFG randomWordDerive g (map (ReprCFG . unReprString) word)
    randomWordDeriveN n (StringCFG g) word =
        map (ReprString . unReprCFG)
            <$> delegateCFGToIntCFG (randomWordDeriveN n) g (map (ReprCFG . unReprString) word)
    randomWordDeriveScan (StringCFG g) word =
        map (map (ReprString . unReprCFG))
            <$> delegateCFGToIntCFG2 randomWordDeriveScan g (map (ReprCFG . unReprString) word)
