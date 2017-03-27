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

whitespace :: Stream s m Char => ParsecT s u m ()
whitespace = skipMany $ oneOf [' ', '\t']

-- | Modify a parser to skip any following whitespace
lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme p = do whitespace
              x <- p
              whitespace
              return x

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = lexeme . between (char '(')  (char ')')

regexParser :: Stream s m Char => ParsecT s u m (Regex Char)
regexParser = alt <* (choice [void newline, eof] <?> "end of input")

alt :: Stream s m Char => ParsecT s u m (Regex Char)
alt = do r <- lexeme $ sepBy1 (lexeme concatenation) (char '|')
         case r of
             [x] -> return x
             xs -> return $ Alt xs

concatenation :: Stream s m Char => ParsecT s u m (Regex Char)
concatenation = do r <- lexeme $ many1 (lexeme multiplication)
                   case r of
                       [x] -> return x
                       xs -> return $ Concat xs

multiplication :: Stream s m Char => ParsecT s u m (Regex Char)
multiplication = do r <- lexeme unit
                    option r (choice [star, plus, questionMark] <*> return r)

star :: Stream s m Char => ParsecT s u m (Regex Char -> Regex Char)
star = lexeme (char '*') *> return Star

plus :: Stream s m Char => ParsecT s u m (Regex Char -> Regex Char)
plus = lexeme (char '+') *> return Plus

questionMark :: Stream s m Char => ParsecT s u m (Regex Char -> Regex Char)
questionMark = lexeme(char '?') *> return QuestionMark

unit :: Stream s m Char => ParsecT s u m (Regex Char)
unit = parens alt <|> lexeme symbolOrEmpty

symbolOrEmpty :: Stream s m Char => ParsecT s u m (Regex Char)
symbolOrEmpty = char '<' *> choice [empty, Lit <$> lit] <?> "identifier or empty"

symbol :: Stream s m Char => ParsecT s u m Char
symbol = char '<' *> lit <?> "identifier"

empty :: Stream s m Char => ParsecT s u m (Regex Char)
empty = char '>' *> pure Empty

lit :: Stream s m Char => ParsecT s u m Char
lit = noneOf ['\"', '<', '>', '(', ')', '*', '+', '?', '|']
                <* char '>' <?> "quote at the end of symbol"
