{-# LANGUAGE FlexibleContexts #-}

module Grammar.Random
( MC
, Seed
, uniformInt
, uniform
, uniforms
, sampleExp
, getGen
, pickRandom
, randomSymExpand
, randomSentExpand
, evalGrammar
) where

-- system imports
import System.Random
import Control.Monad.State
import qualified Data.Map as M

-- local imports
import Grammar
--import Grammar.ParseTree

type Seed = Int

type MC = State StdGen

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
          => a  -- ^ The distribution
          -> m a
sampleExp lambda = do xi <- uniform
                      return $ (-lambda) * log xi

pickRandom :: Ord a => CSG a -> Symbol a -> MC (Sentention a)
pickRandom ps sym = let sententions = apply ps sym
                        n = length sententions
                     in do ran <- uniformInt 0 n
                           return $ sententions !! ran

--randomSymDerive :: Ord a => CSG a -> Symbol a -> MC (ParseTree a)
--randomSymDerive (CSG prods) start = 
--
--randomSentDerive :: Ord a => CSG a -> Sentention a -> MC (ParseTree a)
--randomSentDerive grammar start = undefined

randomSymExpand :: Ord a => CSG a -> Symbol a -> MC (Sentention a)
randomSymExpand grammar@(CSG prods) sym =
    if sym `M.member` prods
    then do sent <- pickRandom grammar sym
            return sent
    else return $ Sentention [sym]

randomSentExpand :: Ord a => CSG a -> Sentention a -> MC (Sentention a)
randomSentExpand g (Sentention syms) = do sents <- sequence $ map (randomSymExpand g) syms
                                          return $ concatSent sents

evalGrammar :: MC a -> Int -> a
evalGrammar obj seed = let initialGen = mkStdGen seed
                        in evalState obj initialGen
