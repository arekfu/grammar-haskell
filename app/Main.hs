module Main where

import Grammar
import Grammar.Random

exampleKeyValue :: [(Char, [String])]
exampleKeyValue = [ ('E', ["E+E", "E*E", "(E)", "I"])
                  , ('I', ["a", "b", "Ia", "Ib", "I0", "I1"])
                  ]

exampleGrammar :: CSG Char
exampleGrammar = productionsToGrammar exampleKeyValue

main :: IO ()
main = do print exampleGrammar
          let expander = randomSentExpand exampleGrammar
          let expansion = return (Sentential [S 'E']) >>= expander >>= expander >>= expander >>= expander >>= expander
          let string = evalGrammar expansion 121333
          print string
