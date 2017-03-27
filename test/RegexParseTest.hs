module RegexParseTest
( runTests
) where

-- system imports
import Test.QuickCheck
import Text.Parsec hiding (Empty)
import Data.Coerce

-- local imports
import SymbolsTest (NonTerminal(..))
import RegexTest (ARegex(..))
import Grammar.Regex
import Grammar.Regex.Parse

prop_parseShowIdempotence :: ARegex NonTerminal -> Property
prop_parseShowIdempotence (ARegex r) =
    let r' = coerce r
        rstr = showRegexQuoted r'
     in case parse regexParser "parseShowIdempotence" rstr of
            Left parseError -> counterexample ("Parsing failed\n  regex: " ++ rstr ++ "\n  error: " ++ show parseError) False
            Right r'' -> simplify r' === simplify r''


return []
runTests :: IO Bool
runTests = $quickCheckAll
