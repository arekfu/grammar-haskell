{-|
Module      : Grammar.Regex.Random
Description : Random expansion of the Regex datatype.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD3
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module exports functions to perform random expansion of regular
expressions.
-}


module Grammar.Regex.Random
( randomExpandRegex
) where

-- local imports
import Grammar.Regex
import Grammar.MC

-------------------------------
--  randomly expand regexes  --
-------------------------------

expConstant :: Double
expConstant = 3.0

{- | Perform random expansion of a 'Regex' and return a string in the resulting
     language. The string is actually a list of symbols and is returned in the
     'Grammar.MC.MC' monad, because it is a random value.
-}
randomExpandRegex :: Regex a -> MC [a]
randomExpandRegex Empty = return []
randomExpandRegex (Lit x) = return [x]
randomExpandRegex (Concat xs) = concat <$> mapM randomExpandRegex xs
randomExpandRegex (Alt xs) = randomExpandRegex =<< pickRandom xs
randomExpandRegex (Star r) = do xi <- sampleExp expConstant
                                expanded <- randomExpandRegex r
                                let n = round xi
                                return $ concat $ replicate n expanded
randomExpandRegex (Plus r) = do xi <- sampleExp expConstant
                                expanded <- randomExpandRegex r
                                let n = 1 + round xi
                                return $ concat $ replicate n expanded
randomExpandRegex (QuestionMark r) = do xi <- uniform :: MC Double
                                        if xi < 0.5 then randomExpandRegex r else return []
