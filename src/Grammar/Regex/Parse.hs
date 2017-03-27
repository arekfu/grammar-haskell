module Grammar.Regex.Parse
( regexParser
) where

-- system imports
import Text.Parsec hiding (Empty)
import Control.Monad (void)

-- local imports
import Grammar.Regex

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = between (char '(')  (char ')')

regexParser :: Stream s m Char => ParsecT s u m (Regex Char)
regexParser = alt <* (choice [void newline, eof] <?> "end of input")

alt :: Stream s m Char => ParsecT s u m (Regex Char)
alt = do r <- sepBy1 concatenation (char '|')
         case r of
             [x] -> return x
             xs -> return $ Alt xs

concatenation :: Stream s m Char => ParsecT s u m (Regex Char)
concatenation = do r <- many1 multiplication
                   case r of
                       [x] -> return x
                       xs -> return $ Concat xs

multiplication :: Stream s m Char => ParsecT s u m (Regex Char)
multiplication = do r <- unit
                    option r (choice [star, plus, questionMark] <*> return r)

star :: Stream s m Char => ParsecT s u m (Regex Char -> Regex Char)
star = char '*' *> return Star

plus :: Stream s m Char => ParsecT s u m (Regex Char -> Regex Char)
plus = char '+' *> return Plus

questionMark :: Stream s m Char => ParsecT s u m (Regex Char -> Regex Char)
questionMark = char '?' *> return QuestionMark

unit :: Stream s m Char => ParsecT s u m (Regex Char)
unit = parens alt <|> symbol

symbol :: Stream s m Char => ParsecT s u m (Regex Char)
symbol = char '<' *> choice [empty, lit] <?> "identifier"

empty :: Stream s m Char => ParsecT s u m (Regex Char)
empty = char '>' *> pure Empty <* spaces

lit :: Stream s m Char => ParsecT s u m (Regex Char)
lit = Lit <$> noneOf ['\"', '<', '>', '(', ')', '*', '+', '?', '|']
                <* char '>' <* spaces <?> "quote at the end of symbol"
