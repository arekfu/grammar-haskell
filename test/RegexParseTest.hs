module RegexParseTest
( runTests
) where

-- system imports
import Test.QuickCheck
import Text.Parsec hiding (Empty)
import Data.Coerce

-- local imports
import SymbolsTest (NonTerminal(..))
import RegexTest (ARegex(..), AQuotingPolicy(..))
import Grammar.Regex
import Grammar.Regex.Parse

prop_parseShowIdempotence :: AQuotingPolicy -> ARegex NonTerminal -> Property
prop_parseShowIdempotence (AQuotingPolicy q) (ARegex r) =
    let r' = coerce r
        rstr = runQuoted (showRegex r') q
     in parseShowIdempotence q rstr r'

parseShowIdempotence :: QuotingPolicy -> String -> Regex Char -> Property
parseShowIdempotence q@NoQuoting rstr r = not (isRegexEmpty r) ==> assertIdempotence q rstr r
parseShowIdempotence q@(Quoting _ _) rstr r = assertIdempotence q rstr r

assertIdempotence :: QuotingPolicy -> String -> Regex Char -> Property
assertIdempotence q rstr r =
        case runParser regexParser q "parseShowIdempotence" rstr of
            Left parseError -> counterexample ("Parsing failed\n  regex: " ++ rstr ++ "\n  error: " ++ show parseError) False
            Right r' -> simplify r === simplify r'

return []
runTests :: IO Bool
runTests = $quickCheckAll
