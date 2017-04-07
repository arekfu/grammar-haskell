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
, Sized
, Seed
, evalMC
, evalMCSized
-- * Sampling random numbers
, getGen
, putGen
, getSize
, putSize
, scaleSize
, getSizeMC
, putSizeMC
, scaleSizeMC
, uniform
, uniformInt
, sampleExp
, sampleSizedExp
, pickRandom
-- * Reexported from 'Control.Monad.State'
, lift
) where

-- system imports
import Control.Monad.State
import System.Random.TF
import System.Random.TF.Gen
import System.Random.TF.Instances
import Data.Int
import Data.Foldable (Foldable, length, toList)

-- local imports
import Grammar.Size

--------------------
--  the MC monad  --
--------------------

-- | Just a type alias for the PRNG seed.
type Seed = Int

-- | The 'MC' type is just an alias for a monad transformer stack. Yes, 'MC'
--   stands for Monte Carlo.
type MC = StateT TFGen (State Size)

type Sized = State Size

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
                             in evalState (evalStateT obj initialGen) size

-----------------------------------------------
--  some machinery to sample random numbers  --
-----------------------------------------------

-- | Extract the random-number generator.
getGen :: (RandomGen g, MonadState g m) => m g
getGen = get

-- | Update the random-number generator.
putGen :: (RandomGen g, MonadState g m) => g -> m ()
putGen = put

-- | Extract the current size.
getSize :: MonadState Size m => m Size
getSize = get

-- | Update the current size.
putSize :: MonadState Size m => Size -> m ()
putSize = put

-- | Scale the current size by a given factor.
scaleSize :: MonadState Size m => Size -> m ()
scaleSize scaling = do size <- get
                       put $ size*scaling

-- | Extract the current size.
getSizeMC :: MC Size
getSizeMC = lift get

-- | Update the current size.
putSizeMC :: Size -> MC ()
putSizeMC size = lift (put size)

-- | Scale the current size by a given factor.
scaleSizeMC :: Size -> MC ()
scaleSizeMC scaling = do size <- getSizeMC
                         putSizeMC $ size*scaling

-- | Return a uniformly distributed 'Fractional' random number between 0 and 1.
--   The interval bounds may or may not be included.
uniform :: (RandomGen g, Fractional a, MonadState g m) => m a
uniform = do
    gen <- getGen
    let (i, gen') = randomR (1, maxBound::Int64) gen
    let xi = fromIntegral (i::Int64) * oneOverMaxInt64
    putGen gen'
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
    putGen gen'
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


-- | Sample from an exponential distribution and take the current size as the
--   distribution parameter.
sampleSizedExp :: MC Double
sampleSizedExp = do xi <- uniform
                    lambda <- getSizeMC
                    let lambda' = realToFrac lambda
                    return $ (-lambda') * log xi


-- | Pick a random element from a Foldable container.
pickRandom :: (RandomGen g, MonadState g m, Foldable t) => t a -> m a
pickRandom set = let l = toList set
                     n = length l
                  in do ran <- uniformInt 0 (n-1)
                        return $ l !! ran
