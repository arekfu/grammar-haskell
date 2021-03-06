module CFGParseTest
( runTests
) where

-- system imports
import Test.QuickCheck
import Text.Parsec hiding (Empty)

-- local imports
import CFGTest (ACharCFG(..))
import RegexTest (AQuotingPolicy(..))
import Grammar.CFG
import Grammar.CFG.Parse
import Grammar.Regex

prop_parseShowIdempotence :: AQuotingPolicy -> ACharCFG -> Property
prop_parseShowIdempotence (AQuotingPolicy q@NoQuoting) (ACharCFG g) = parseShowIdempotence q g
prop_parseShowIdempotence (AQuotingPolicy q@(Quoting l r)) (ACharCFG g) =
    ReprChar l `isNotInGrammar` g && ReprChar r `isNotInGrammar` g ==> parseShowIdempotence q g

parseShowIdempotence :: QuotingPolicy -> CharCFG -> Property
parseShowIdempotence q g =
    let gstr = runQuoted (showGrammar g) q
        ReprChar start = startSymbol g
     in case runParser cfgParser q "parseShowIdempotence" gstr of
            Left parseError -> counterexample ("Parsing failed\n  grammar:\n" ++ gstr ++ "\n  error: " ++ show parseError) False
            Right kvs -> let g' = regexesToCharCFG start kvs
                          in simplifyCharCFG g === simplifyCharCFG g'


return []
runTests :: IO Bool
runTests = $quickCheckAll
