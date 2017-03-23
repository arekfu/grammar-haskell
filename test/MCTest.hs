module MCTest
( runTests
) where

-- system imports
import Test.QuickCheck

-- local imports
import Grammar.MC

prop_uniformBounds :: Positive (Large Seed) -> Property
prop_uniformBounds (Positive (Large seed)) =
    let xi :: Double
        xi = evalMC uniform seed
     in xi>0.0 .&&. xi<=1.0

prop_expBound :: Positive (Large Seed) -> Positive Double -> Bool
prop_expBound (Positive (Large seed)) (Positive lambda) =
    let xi :: Double
        xi = evalMC (sampleExp lambda) seed
     in xi>0.0

return []
runTests :: IO Bool
runTests = $quickCheckAll
