{-# LANGUAGE ScopedTypeVariables #-}

module RegexTest
( ARegex(..)
, runTests
) where

-- system imports
import Test.QuickCheck
import Data.Coerce

-- local imports
import SymbolsTest (NonTerminal)
import Grammar.Regex


newtype ARegex a = ARegex { unARegex :: Regex a } deriving (Eq, Ord, Show)


instance Arbitrary a => Arbitrary (ARegex a) where
    arbitrary = sized aRegexSized
        where aRegexSized :: Arbitrary a => Int -> Gen (ARegex a)
              aRegexSized n = frequency $ zip [5, 2, 1, 1, 1, 1] $ map (ARegex <$>)
                  [ Lit <$> arbitrary
                  , (Concat . coerce) <$> resize (n `div` 2) (listOf1 (arbitrary :: Gen (ARegex a)) `suchThat` (\l -> length l>1))
                  , (Alt . coerce) <$> resize (n `div` 2) (listOf1 (arbitrary :: Gen (ARegex a)) `suchThat` (\l -> length l>1))
                  , (Star . coerce) <$> aRegexSized n
                  , (Plus . coerce) <$> aRegexSized n
                  , (QuestionMark . coerce) <$> aRegexSized n
                  ]


runTests :: IO ()
runTests = do putStrLn "Generating some random regexes..."
              rs <- sample' (arbitrary :: Gen (ARegex NonTerminal))
              mapM_ (print . unARegex) rs
