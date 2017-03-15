module Main where

import Grammar
import Grammar.Random

import System.Clock

exampleKeyValue :: [(Char, [String])]
exampleKeyValue = let initialChars = map (:[]) ['a'..'c']
                      chars = ['a'..'c'] ++ ['0'..'3']
                      expansions = map (\c -> ['I', c]) chars
                   in [ ('E', ["E+E", "E*E", "(E)", "I"])
                      , ('I', initialChars ++ expansions)
                      ]

exampleGrammar :: CharCFG
exampleGrammar = productionsToCharCFG 'E' exampleKeyValue

main :: IO ()
main = do print exampleGrammar
          let expansion = randomGrammarDeriveScan exampleGrammar
          TimeSpec _ seed <- getTime Realtime
          let strings = take 20 $ evalGrammar expansion $ fromIntegral seed
          mapM_ putStrLn strings
