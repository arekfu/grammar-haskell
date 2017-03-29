{-|
Module      : Grammar.MC
Description : Definition of the MC monad.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD3
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module exports the 'MC' (Monte Carlo) monad, which can be used to stream
the pseudo-random number generator seed through different applications.
-}

module Grammar.MC
(
-- * The 'MC' monad
  MC
, Seed
, evalMC
-- * Sampling random numbers
, getGen
, uniform
, uniformInt
, sampleExp
, pickRandom
) where

-- system imports
import Control.Monad.State
import System.Random.TF
import System.Random.TF.Gen
import System.Random.TF.Instances
import Data.Int
import Data.Foldable (Foldable, length, toList)

--------------------
--  the MC monad  --
--------------------

-- | Just a type alias for the PRNG seed.
type Seed = Int

-- | The 'MC' type is just an alias for the 'State' 'StdGen' monad. Yes, 'MC' stands for Monte Carlo.
type MC = State TFGen

oneOverMaxInt64 :: Fractional a => a
oneOverMaxInt64 = 1.0 / fromIntegral (maxBound::Int64)

{- | How do I escape from the 'MC' monad? Just call evalGrammar and supply a
   starting seed for the pseudo-random number generator.
-}
evalMC :: MC a -- ^ the computation to perform
       -> Seed -- ^ the starting seed
       -> a    -- ^ the computation result
evalMC obj seed = let initialGen = mkTFGen seed
                   in evalState obj initialGen

-----------------------------------------------
--  some machinery to sample random numbers  --
-----------------------------------------------

-- | Extract the random-number generator.
getGen :: (RandomGen g, MonadState g m) => m g
getGen = get

-- | Return a uniformly distributed 'Fractional' random number between 0 and 1.
--   The interval bounds may or may not be included.
uniform :: (RandomGen g, Fractional a, MonadState g m) => m a
uniform = do
    gen <- getGen
    let (i, gen') = randomR (1, maxBound::Int64) gen
    let xi = fromIntegral (i::Int64) * oneOverMaxInt64
    put gen'
    return xi

-- | Return a uniformly distributed integer between the specified minimum and
--   maximum values (included).
uniformInt :: (RandomGen g, Random a, Integral a, MonadState g m)
           => a     -- ^ the minimum value
           -> a     -- ^ the maximum value
           -> m a   -- ^ the sampled value
uniformInt minVal maxVal = do
    gen <- getGen
    let (xi, gen') = randomR (minVal, maxVal) gen
    put gen'
    return xi

-- | Sample from an exponential distribution of the form
-- @
-- f(x) = exp(-&#x3BB; x)/&#x3BB;
-- @
sampleExp :: (RandomGen g, MonadState g m)
          => Double  -- ^ The distribution mean
          -> m Double
sampleExp lambda = do xi <- uniform
                      return $ (-lambda) * log xi

-- | Pick a random element from a Foldable container.
pickRandom :: Foldable t => t a -> MC a
pickRandom set = let l = toList set
                     n = length l
                  in do ran <- uniformInt 0 (n-1)
                        return $ l !! ran

