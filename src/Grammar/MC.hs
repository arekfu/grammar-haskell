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
, uniforms
, sampleExp
, pickRandom
) where

-- system imports
import Control.Monad.State
import System.Random
import Data.Foldable (Foldable, length, toList)

--------------------
--  the MC monad  --
--------------------

-- | Just a type alias for the PRNG seed.
type Seed = Int

-- | The 'MC' type is just an alias for the 'State' 'StdGen' monad. Yes, 'MC' stands for Monte Carlo.
type MC = State StdGen

{- | How do I escape from the 'MC' monad? Just call evalGrammar and supply a
   starting seed for the pseudo-random number generator.
-}
evalMC :: MC a -- ^ the computation to perform
       -> Seed -- ^ the starting seed
       -> a    -- ^ the computation result
evalMC obj seed = let initialGen = mkStdGen seed
                   in evalState obj initialGen

-----------------------------------------------
--  some machinery to sample random numbers  --
-----------------------------------------------

getGen :: MonadState StdGen m => m StdGen
getGen = get

uniform :: (Random a, Fractional a, MonadState StdGen m) => m a
uniform = do
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

uniforms :: (Random a, Fractional a, MonadState StdGen m)
         => Int
         -> m [a]
uniforms n = replicateM n uniform

-- | Sample from an exponential distribution of the form
-- @
-- f(x) = exp(-&#x3BB; x)/&#x3BB;
-- @
sampleExp :: (Random a, Floating a, MonadState StdGen m)
          => a  -- ^ The distribution mean
          -> m a
sampleExp lambda = do xi <- uniform
                      return $ (-lambda) * log xi

-- | Pick a random element from a Foldable container.
pickRandom :: Foldable t => t a -> MC a
pickRandom set = let l = toList set
                     n = length l
                  in do ran <- uniformInt 0 (n-1)
                        return $ l !! ran

