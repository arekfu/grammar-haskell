{-# LANGUAGE ScopedTypeVariables #-}

module RegexTest
( ARegex(..)
, runTests
, printExamples
) where

-- system imports
import Test.QuickCheck
import Test.QuickCheck.Function
import Data.Foldable
import Data.Coerce
import Data.Monoid (Sum, Endo(..), Dual(..), appEndo, getDual)

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

    shrink (ARegex Empty)            = []
    shrink (ARegex (Lit _))          = [ARegex Empty]
    shrink (ARegex (Concat rs))      = [ARegex Empty] ++ coerce rs
    shrink (ARegex (Alt rs))         = [ARegex Empty] ++ coerce rs
    shrink (ARegex (Star r))         = [ARegex Empty, ARegex r]
    shrink (ARegex (Plus r))         = [ARegex Empty, ARegex r]
    shrink (ARegex (QuestionMark r)) = [ARegex Empty, ARegex r]

prop_functorLaw1 :: (Eq a, Show a) => ARegex a -> Property
prop_functorLaw1 (ARegex regex) =
    fmap id regex === regex


prop_functorLaw2 :: (Eq c, Show c) => Fun a b -> Fun b c -> ARegex a -> Property
prop_functorLaw2 (Fun _ f) (Fun _ g) (ARegex regex) =
    fmap (g . f) regex === fmap g (fmap f regex)


prop_foldableLaw1 :: ARegex (Sum Int) -> Property
prop_foldableLaw1 (ARegex regex) =
    foldMap id regex === fold regex


prop_foldableLaw2 :: Fun Char (Fun Int Int) -> Int -> ARegex Char -> Property
prop_foldableLaw2 (Fun _ f) z (ARegex regex) =
    let f' = apply . f
     in foldr f' z regex === appEndo (foldMap (Endo . f') regex) z

prop_foldableLaw3 :: Fun Int (Fun Char Int) -> Int -> ARegex Char -> Property
prop_foldableLaw3 (Fun _ f) z (ARegex regex) =
    let f' = apply . f
     in foldl f' z regex === appEndo (getDual (foldMap (Dual . Endo . flip f') regex)) z



printExamples :: IO ()
printExamples = do putStrLn "Generating some random regexes..."
                   rs <- sample' (arbitrary :: Gen (ARegex NonTerminal))
                   mapM_ (print . unARegex) rs


return []
runTests :: IO Bool
runTests = $quickCheckAll
