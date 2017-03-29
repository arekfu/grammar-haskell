{-|
Module      : Grammar.CFG.Parse
Description : Parsec parser for CFG and CFG-derived datatypes.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD3
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module contains a 'Text.Parsec' parser that can be used to construct
context-free grammars ('CFG') from their representation in some kind of
extended Backus-Naur form, in which the right-hand-side of the expression is
actually a regular expression over the language alphabet.

Parsing and showing (using 'showGrammarWith') should be inverse of each other,
provided that the same quoting policy is applied.

Here is an example context-free grammar for a simple expression language:

    >>> let exampleGrammarString = "\
    ... \ 'E' := 'E' '+' 'E' | 'E' '*' 'E' | '(' 'E' ')' | 'I'\n\
    ... \ 'I' := 'a' | 'b' | 'c' | 'I' 'a' | 'I' 'b' | 'I' 'c'"

    >>> parseCharCFG (Quoting '\'' '\'') 'E' exampleGrammarString
    CharCFG (CFG 'E' (IntCFG [('E',Alt [Concat [Lit 'E',Lit '+',Lit 'E'],Concat [Lit 'E',Lit '*',Lit 'E'],Concat [Lit '(',Lit 'E',Lit ')'],Lit 'I']),('I',Alt [Lit 'a',Lit 'b',Lit 'c',Concat [Lit 'I',Lit 'a'],Concat [Lit 'I',Lit 'b'],Concat [Lit 'I',Lit 'c']])] 8 2 (fromList [(0,Alt [Concat [Lit 0,Lit 5,Lit 0],Concat [Lit 0,Lit 4,Lit 0],Concat [Lit 2,Lit 0,Lit 3],Lit 1]),(1,Alt [Lit 6,Lit 7,Lit 8,Concat [Lit 1,Lit 6],Concat [Lit 1,Lit 7],Concat [Lit 1,Lit 8]])])) (fromList [('(',2),(')',3),('*',4),('+',5),('E',0),('I',1),('a',6),('b',7),('c',8)]) "EI()*+abc")
-}

module Grammar.CFG.Parse
( cfgParser
, parseCharCFG
) where

-- system imports
import Text.Parsec hiding (Empty)
import Control.Monad (void)
import Debug.Trace

-- local imports
import Grammar.CFG
import Grammar.Regex
import Grammar.Regex.Parse

ruleParser :: Stream s m Char => ParsecT s QuotingPolicy m (Char, Regex Char)
ruleParser = do sym <- lexeme symbol
                void $ lexeme $ string ":="
                regex <- lexeme regexParser
                return (sym, regex)

-- | A Parsec parser for a context-free grammar over 'Char's, in the form of an
--   association list of @('Char', 'Regex' 'Char')@ pairs.
cfgParser :: Stream s m Char => ParsecT s QuotingPolicy m [(Char, Regex Char)]
cfgParser = many ruleParser

{- | Parse a string specification of a context-free grammar and construct a
     'CharCFG' object based on a starting symbol and a quoting policy.
-}
parseCharCFG :: QuotingPolicy   -- ^ the quoting policy
             -> Char            -- ^ the start symbol
             -> String          -- ^ the grammar specification
             -> CharCFG         -- ^ the resulting grammar
parseCharCFG q start s =
    case runParser cfgParser q "parseShowIdempotence" s of
        Left parseError -> error $ show parseError
        Right kvs -> simplifyCharCFG $ regexesToCharCFG start $ trace (show kvs) kvs
