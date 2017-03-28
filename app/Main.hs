module Main where

import Grammar
import Grammar.CFG
import Grammar.MC

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

digitsKeyValue :: [(String, [[String]])]
digitsKeyValue = [ ("<S>", [["-", "<FN>"], ["<FN>"]])
                 , ("<FN>", [["<DL>"], ["<DL>", ".", "<DL>"]])
                 , ("<DL>", [["<D>"], ["<D>", "<DL>"]])
                 , ("<D>", [["0"], ["1"], ["2"], ["3"], ["4"], ["5"], ["6"], ["7"], ["8"], ["9"]])
                 ]

digitsGrammar :: StringCFG
digitsGrammar = productionsToStringCFG "<S>" digitsKeyValue

main :: IO ()
main = do
          -- example grammar
          putStrLn $ showGrammar exampleGrammar
          let expansion = randomGrammarDeriveScan exampleGrammar
          TimeSpec _ seed <- getTime Realtime
          let strings = take 20 $ evalMC expansion $ fromIntegral seed
          mapM_ (putStrLn . concatMap (showSymbol NoQuoting)) strings
          putStrLn ""
          -- digits grammar
          putStrLn $ showGrammar digitsGrammar
          let digitExpansion = randomGrammarDerive digitsGrammar
          putStrLn $ concatMap (showSymbol NoQuoting) $ evalMC digitExpansion $ fromIntegral seed
          putStrLn ""
          -- examine the structureof the regexes
          print $ productions digitsGrammar (ReprString "<D>")
