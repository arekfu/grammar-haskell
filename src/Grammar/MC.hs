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
, evalMCSized
-- * Sampling random numbers
, getGen
, putGen
, getSize
, putSize
, scaleSize
, uniform
, uniformInt
, sampleExp
, sampleSizedExp
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

-- | A type alias for sized random expansion
type Size = Double

-- | The 'MC' type is just an alias for the 'State' 'StdGen' monad. Yes, 'MC' stands for Monte Carlo.
type MC = State (TFGen, Size)

oneOverMaxInt64 :: Fractional a => a
oneOverMaxInt64 = 1.0 / fromIntegral (maxBound::Int64)

{- | How do I escape from the 'MC' monad? Just call 'evalMC' and supply a
   starting seed for the pseudo-random number generator.
-}
evalMC :: MC a -- ^ the computation to perform
       -> Seed -- ^ the starting seed
       -> a    -- ^ the computation result
evalMC = let defaultSize = 5.0 in evalMCSized defaultSize

{- | If you want more control over the size of the generated expressions, you
     can use 'evalMCSized' to escape from the 'MC' monad. This allows you to
     provide an expected size for the resulting expressions. Note that @evalMC
     == evalMCSized 5.0@.
-}
evalMCSized :: Size -- ^ the size
            -> MC a -- ^ the computation to perform
            -> Seed -- ^ the starting seed
            -> a    -- ^ the computation result
evalMCSized size obj seed = let initialGen = mkTFGen seed
                             in evalState obj (initialGen, size)

-----------------------------------------------
--  some machinery to sample random numbers  --
-----------------------------------------------

-- | Extract the random-number generator.
getGen :: (RandomGen g, MonadState (g, a) m) => m g
getGen = fst <$> get

-- | Update the random-number generator.
putGen :: (RandomGen g, MonadState (g, a) m) => g -> m ()
putGen gen = do (_, other) <- get
                put (gen, other)

-- | Extract the current size.
getSize :: MonadState (a, Size) m => m Size
getSize = snd <$> get

-- | Update the current size.
putSize :: MonadState (a, Size) m => Size -> m ()
putSize size = do (other, _) <- get
                  put (other, size)

-- | Scale the current size by a given factor.
scaleSize :: MonadState (a, Size) m => Size -> m ()
scaleSize scaling = do (other, size) <- get
                       put (other, size*scaling)

-- | Return a uniformly distributed 'Fractional' random number between 0 and 1.
--   The interval bounds may or may not be included.
uniform :: (RandomGen g, Fractional a, MonadState (g, b) m) => m a
uniform = do
    gen <- getGen
    let (i, gen') = randomR (1, maxBound::Int64) gen
    let xi = fromIntegral (i::Int64) * oneOverMaxInt64
    putGen gen'
    return xi

-- | Return a uniformly distributed integer between the specified minimum and
--   maximum values (included).
uniformInt :: (RandomGen g, Random a, Integral a, MonadState (g, b) m)
           => a     -- ^ the minimum value
           -> a     -- ^ the maximum value
           -> m a   -- ^ the sampled value
uniformInt minVal maxVal = do
    gen <- getGen
    let (xi, gen') = randomR (minVal, maxVal) gen
    putGen gen'
    return xi

-- | Sample from an exponential distribution of the form
-- @
-- f(x) = exp(-&#x3BB; x)/&#x3BB;
-- @
sampleExp :: (RandomGen g, MonadState (g, a) m)
          => Double  -- ^ The distribution mean
          -> m Double
sampleExp lambda = do xi <- uniform
                      return $ (-lambda) * log xi


-- | Sample from an exponential distribution and take the current size as the
--   distribution parameter.
sampleSizedExp :: (RandomGen g, MonadState (g, Size) m)
               => m Double
sampleSizedExp = do xi <- uniform
                    lambda <- getSize
                    return $ (-lambda) * log xi


-- | Pick a random element from a Foldable container.
pickRandom :: (RandomGen g, MonadState (g, b) m, Foldable t) => t a -> m a
pickRandom set = let l = toList set
                     n = length l
                  in do ran <- uniformInt 0 (n-1)
                        return $ l !! ran
