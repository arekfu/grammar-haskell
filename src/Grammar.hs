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
-- * Context-free grammars
-- ** The 'Grammar' typeclass
  Grammar(..)
, Repr(..)
-- ** Context-free grammars over alphabets of integers
, IntCFG
, productionsToIntCFG
-- ** Context free grammars over alphabets of arbitrary types
, CFG
, productionsToCFG
-- ** Context-free grammars over alphabets of specific types
, CharCFG
, productionsToCharCFG
, parseCharCFG
, StringCFG
, productionsToStringCFG
-- ** Pretty-printing parts of a grammar
-- $examplegrammar
, showSymbol
, showWord
, showProductions
, showGrammar
, showGrammarUnquoted
, showGrammarQuoted
, showGrammarChevrons
, QuotingPolicy(..)
, quote
-- * Regular expressions
, Regex
, showRegex
, showRegexUnquoted
, showRegexQuoted
, showRegexChevrons
-- * Random grammar derivation
, MC
, evalMC
, randomGrammarDerive
, randomGrammarDeriveScan
) where

-- local imports
import Grammar.CFG
import Grammar.CFG.Parse
import Grammar.CFG.Random
import Grammar.Regex
import Grammar.MC

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
