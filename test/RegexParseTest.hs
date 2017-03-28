module RegexParseTest
( runTests
) where

-- system imports
import Test.QuickCheck
import Text.Parsec hiding (Empty)
import Data.Coerce

-- local imports
import SymbolsTest (NonTerminal(..))
import RegexTest (ARegex(..), AQuoting(..))
import Grammar.Regex
import Grammar.Regex.Parse

prop_parseShowIdempotence :: AQuoting -> ARegex NonTerminal -> Property
prop_parseShowIdempotence (AQuoting q) (ARegex r) =
    let r' = coerce r
        rstr = showRegexWith q r'
     in hasQuoting q || not (isRegexEmpty r) ==>
        case runParser regexParser q "parseShowIdempotence" rstr of
            Left parseError -> counterexample ("Parsing failed\n  regex: " ++ rstr ++ "\n  error: " ++ show parseError) False
            Right r'' -> simplify r' === simplify r''


return []
runTests :: IO Bool
runTests = $quickCheckAll
