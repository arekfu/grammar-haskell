{-|
Module      : Grammar.Regex.Parse
Description : Parsec parser for the Regex datatype.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD3
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module contains a 'Text.Parsec' parser that can be used to construct
'Regex'es from their 'String' representation. We try (and test!) to enforce
that parsing and showing (using 'showRegexWith') are inverse of each other,
provided that the same quoting policy is applied.
-}


module Grammar.Regex.Parse
( regexParser
, whitespace
, lexeme
, symbol
) where

-- system imports
import Text.Parsec hiding (Empty)
import Control.Monad (void)

-- local imports
import Grammar.Regex

{- | Skip any amount of whitespace. It differs from 'Text.Parsec.Char.spaces'
   because the latter skips all characters for which 'isSpace' is 'True', which
   includes newlines.
-}
whitespace :: Stream s m Char => ParsecT s QuotingPolicy m ()
whitespace = skipMany $ oneOf [' ', '\t']

-- | Modify a parser to skip any following whitespace.
lexeme :: Stream s m Char => ParsecT s QuotingPolicy m a -> ParsecT s QuotingPolicy m a
lexeme p = do whitespace
              x <- p
              whitespace
              return x

parens :: Stream s m Char => ParsecT s QuotingPolicy m a -> ParsecT s QuotingPolicy m a
parens = lexeme . between (char '(')  (char ')')

-- | A Parsec parser for 'Regex'es.
regexParser :: Stream s m Char => ParsecT s QuotingPolicy m (Regex Char)
regexParser = alt <* (choice [void newline, eof] <?> "end of input")

alt :: Stream s m Char => ParsecT s QuotingPolicy m (Regex Char)
alt = do r <- lexeme $ sepBy1 (lexeme concatenation) (char '|')
         case r of
             [x] -> return x
             xs -> return $ Alt xs

concatenation :: Stream s m Char => ParsecT s QuotingPolicy m (Regex Char)
concatenation = do r <- lexeme $ many1 (lexeme multiplication)
                   case r of
                       [x] -> return x
                       xs -> return $ Concat xs

multiplication :: Stream s m Char => ParsecT s QuotingPolicy m (Regex Char)
multiplication = do r <- lexeme unit
                    option r (choice [star, plus, questionMark] <*> return r)

star :: Stream s m Char => ParsecT s QuotingPolicy m (Regex Char -> Regex Char)
star = lexeme (char '*') *> return Star

plus :: Stream s m Char => ParsecT s QuotingPolicy m (Regex Char -> Regex Char)
plus = lexeme (char '+') *> return Plus

questionMark :: Stream s m Char => ParsecT s QuotingPolicy m (Regex Char -> Regex Char)
questionMark = lexeme(char '?') *> return QuestionMark

unit :: Stream s m Char => ParsecT s QuotingPolicy m (Regex Char)
unit = parens alt <|> lexeme symbolOrEmpty

symbolOrEmpty :: Stream s m Char => ParsecT s QuotingPolicy m (Regex Char)
symbolOrEmpty =
    do quoting <- getState
       case quoting of
           Quoting left _ -> void $ char left
           NoQuoting      -> return ()
       choice [empty, Lit <$> lit] <?> "identifier or empty"

-- | Parse a non-empty 'Regex' symbol.
symbol :: Stream s m Char => ParsecT s QuotingPolicy m Char
symbol = do quoting <- getState
            case quoting of
                Quoting left _ -> void $ char left
                NoQuoting      -> return ()
            lit <?> "identifier"

empty :: Stream s m Char => ParsecT s QuotingPolicy m (Regex Char)
empty = do quoting <- getState
           case quoting of
               Quoting _ right -> void $ char right
               NoQuoting       -> parserZero
           return Empty

lit :: Stream s m Char => ParsecT s QuotingPolicy m Char
lit = do quoting <- getState
         case quoting of
             Quoting left right -> parser [left, right] <* void (char right) <?> "quote at the end of symbol"
             NoQuoting          -> parser reservedChars
    where parser sc = noneOf sc <|> (char '\\' *> oneOf sc)
