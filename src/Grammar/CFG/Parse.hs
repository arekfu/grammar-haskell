module Grammar.CFG.Parse
( cfgParser
) where

-- system imports
import Text.Parsec hiding (Empty)
import Control.Monad (void)

-- local imports
import Grammar.Regex
import Grammar.Regex.Parse

ruleParser :: Stream s m Char => ParsecT s Quoting m (Char, Regex Char)
ruleParser = do sym <- lexeme symbol
                void $ lexeme $ string ":="
                regex <- lexeme $ regexParser
                return (sym, regex)

cfgParser :: Stream s m Char => ParsecT s Quoting m [(Char, Regex Char)]
cfgParser = many ruleParser
