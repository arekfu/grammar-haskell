module Main where

import Grammar

exampleKeyValue :: [(Char, [String])]
exampleKeyValue = let initialChars = map (:[]) ['a'..'c']
                      chars = ['a'..'c'] ++ ['0'..'3']
                      expansions = map (\c -> ['I', c]) chars
                   in [ ('E', ["E+E", "E*E", "(E)", "I"])
                      , ('I', initialChars ++ expansions)
                      ]

exampleGrammar :: CharCFG
exampleGrammar = productionsToCharCFG exampleKeyValue

main :: IO ()
main = do print exampleGrammar
          let expansion = randomSentDeriveScan exampleGrammar "E"
          let strings = take 20 $ evalGrammar expansion 121333
          mapM_ putStrLn strings
