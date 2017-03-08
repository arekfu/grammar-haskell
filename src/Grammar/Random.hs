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
, pickRandomSentential
, randomSymExpand
, randomSentExpand
, randomSentDerive
, evalGrammar
) where

-- system imports
import System.Random
import Control.Monad.State
import qualified Data.Map as M
import Data.Foldable (Foldable, length, toList)

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

pickRandom :: Foldable t => t a -> MC a
pickRandom set = let l = toList set
                     n = length l
                  in do ran <- uniformInt 0 (n-1)
                        return $ l !! ran

pickRandomSentential :: Ord a => CSG a -> Symbol a -> MC (Sentential a)
pickRandomSentential ps sym = let sententions = apply ps sym
                               in pickRandom sententions

randomSymExpand :: Ord a => CSG a -> Symbol a -> MC (Sentential a)
randomSymExpand grammar@(CSG prods) sym =
    if sym `M.member` prods
    then do sent <- pickRandomSentential grammar sym
            return sent
    else return $ Sentential [sym]

randomSentExpand :: Ord a => CSG a -> Sentential a -> MC (Sentential a)
randomSentExpand g (Sentential syms) = do sents <- sequence $ map (randomSymExpand g) syms
                                          return $ concatSent sents

randomSentDerive :: (Ord a, Show a) => CSG a -> Sentential a -> MC (Sentential a)
randomSentDerive grammar@(CSG prods) sent =
    do expanded <- randomSentExpand grammar sent
       let isExpanded = all (\ sym -> sym `M.notMember` prods) $ getSyms expanded
       if isExpanded then return expanded else randomSentDerive grammar expanded

evalGrammar :: MC a -> Int -> a
evalGrammar obj seed = let initialGen = mkStdGen seed
                        in evalState obj initialGen
