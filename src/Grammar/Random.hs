{-|
Module      : Grammar.Random
Description : Functions to randomly expand symbols and sentences according to a grammar.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD3
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the relevant tools to sample random sentences from a
context-free grammar. All computations take place in the 'MC' monad, which is
simply a practical way to thread the pseudo-random-number generator (PRNG)
state. Be careful: the expansion is currently totally unbiased -- it will
expand a given nonterminal by assuming that all of the production rules have
equal probability. Depending on the grammar, this may lead the size of the
generated sentence to grow without bound, or to become very large.
-}

{-# LANGUAGE FlexibleContexts #-}

module Grammar.Random
(
-- * The 'MC' monad
  MC
, Seed
, evalGrammar
-- * Randomly expanding symbols and sentences
, randomSymExpand
, randomSentExpand
, randomSentDerive
, randomSentDeriveN
, randomSentDeriveScan
) where

-- system imports
import System.Random
import Control.Monad.State
import Data.Foldable (Foldable, length, toList)

-- local imports
import Grammar.Internal

-- | Just a type alias for the PRNG seed.
type Seed = Int

-- | The 'MC' type is just an alias for the 'State' 'StdGen' monad. Yes, 'MC' stands for Monte Carlo.
type MC = State StdGen

{- | How do I escape from the 'MC' monad? Just call evalGrammar and supply a
   starting seed for the pseudo-random number generator.
-}
evalGrammar :: MC a -- ^ the computation to perform
            -> Seed -- ^ the starting seed
            -> a    -- ^ the computation result
evalGrammar obj seed = let initialGen = mkStdGen seed
                        in evalState obj initialGen

-----------------------------------------------
--  some machinery to sample random numbers  --
-----------------------------------------------

getGen :: MonadState StdGen m => m StdGen
getGen = get

_uniform :: (Random a, Fractional a, MonadState StdGen m) => m a
_uniform = do
    gen <- getGen
    let (xi, gen') = randomR (0.0, 1.0) gen
    put gen'
    return xi

uniformInt :: (Random a, Integral a, MonadState StdGen m) => a -> a -> m a
uniformInt minVal maxVal = do
    gen <- getGen
    let (xi, gen') = randomR (minVal, maxVal) gen
    put gen'
    return xi

_uniforms :: (Random a, Fractional a, MonadState StdGen m)
         => Int
         -> m [a]
_uniforms n = replicateM n _uniform

-- | Sample from an exponential distribution of the form
-- @
-- f(x) = exp(-&#x3BB; x)/&#x3BB;
-- @
_sampleExp :: (Random a, Floating a, MonadState StdGen m)
          => a  -- ^ The distribution
          -> m a
_sampleExp lambda = do xi <- _uniform
                       return $ (-lambda) * log xi

-- | Pick a random element from a Foldable container.
pickRandom :: Foldable t => t a -> MC a
pickRandom set = let l = toList set
                     n = length l
                  in do ran <- uniformInt 0 (n-1)
                        return $ l !! ran

-- | Randomly expand a symbol using one of its production rules.
randomSymExpand :: (Grammar g, Ord (Repr g))
                => g            -- ^ the grammar
                -> Repr g       -- ^ the symbol to expand
                -> MC [Repr g]  -- ^ the resulting sentence
randomSymExpand gr sym = case productions gr sym of
                                [] -> return [sym]
                                sentences  -> pickRandom sentences

-- | Expand a sentence (a sequence of symbols) using randomly selected
--   production rules for each nonterminal.
randomSentExpand :: (Grammar g, Ord (Repr g))
                 => g           -- ^ the grammar
                 -> [Repr g]    -- ^ the sentence to expand
                 -> MC [Repr g] -- ^ the resulting sentence
randomSentExpand g syms = do sents <- sequence $ map (randomSymExpand g) syms
                             return $ concat sents

-- | Recursively and randomly expand a sentence until it consists solely of
--   terminals. WARNING: may produce infinite lists!
randomSentDerive :: (Grammar g, Ord (Repr g))
                 => g           -- ^ the grammar
                 -> [Repr g]    -- ^ the sentence to expand
                 -> MC [Repr g] -- ^ a fully expanded sequence of terminals
randomSentDerive grammar sent =
    do expanded <- randomSentExpand grammar sent
       if sent == expanded
       then return expanded
       else randomSentDerive grammar expanded

-- | Recursively and randomly expand a sentence until it consists solely of
--   terminals or until @n@ expansion steps have been performed, whichever
--   comes first.
randomSentDeriveN :: (Grammar g, Ord (Repr g))
                  => Int            -- ^ the maximum number of expansions
                  -> g              -- ^ the grammar
                  -> [Repr g]       -- ^ the starting sentence
                  -> MC [Repr g]    -- ^ the resulting expansion
randomSentDeriveN 0 _ sent = return sent
randomSentDeriveN n grammar sent = do expanded <- randomSentDerive grammar sent
                                      randomSentDeriveN (n-1) grammar expanded

-- | Recursively and randomly expand a sentence, and return all the
--   intermediate expansion results. WARNING: may produce infinite lists!
randomSentDeriveScan :: (Grammar g, Ord (Repr g))
                     => g               -- ^ the grammar
                     -> [Repr g]        -- ^ the starting sentence
                     -> MC [[Repr g]]   -- ^ the list of all intermediate expansions
randomSentDeriveScan grammar sent =
    do expanded <- randomSentExpand grammar sent
       if sent == expanded
       then return [expanded]
       else liftM (expanded:) (randomSentDeriveScan grammar expanded)
