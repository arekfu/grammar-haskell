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
, pick
-- ** Pretty-printing parts of a grammar
-- $examplegrammar
, showSentence
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
) where

-- local imports
import Grammar.Internal
