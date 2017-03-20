{-|
Module      : Grammar
Description : Export the most useful functions to manipulate context-free grammars.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD3
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module exports the most important datatypes and functions that can be used
to represent and manipulate context-free grammars.
-}
module Grammar
(
-- * The 'Grammar' typeclass
  Grammar(..)
-- ** Pretty-printing parts of a grammar
-- $examplegrammar
, showWord
, showProductions
, showGrammar
-- * Context-free grammars over alphabets of integers
, IntCFG
, productionsToIntCFG
-- * Context free grammars over alphabets of arbitrary types
, CFG
, productionsToCFG
-- * Context-free grammars over alphabets of specific types
, CharCFG
, productionsToCharCFG
, StringCFG
, productionsToStringCFG
-- * Random grammar derivation
, randomGrammarDerive
, randomGrammarDeriveScan
) where

-- local imports
import Grammar.Internal
import Grammar.Random

{- $examplegrammar #examplegrammar#
   We illustrate the pretty-printing functionality with the following example grammar:

   > exampleKeyValue :: [(Char, [String])]
   > exampleKeyValue = let initialChars = map (:[]) ['a'..'c']
   >                       chars = ['a'..'c'] ++ ['0'..'3']
   >                       expansions = map (\c -> ['I', c]) chars
   >                    in [ ('E', ["E+E", "E*E", "(E)", "I"])
   >                       , ('I', initialChars ++ expansions)
   >                       ]
   >
   > exampleGrammar :: CharCFG
   > exampleGrammar = productionsToCharCFG exampleKeyValue
-}
