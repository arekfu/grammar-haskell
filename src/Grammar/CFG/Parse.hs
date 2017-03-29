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

cfgParser :: Stream s m Char => ParsecT s QuotingPolicy m [(Char, Regex Char)]
cfgParser = many ruleParser

parseCharCFG :: QuotingPolicy -> Char -> String -> CharCFG
parseCharCFG q start s =
    case runParser cfgParser q "parseShowIdempotence" s of
        Left parseError -> error $ show parseError
        Right kvs -> simplifyCharCFG $ regexesToCharCFG start $ trace (show kvs) kvs
