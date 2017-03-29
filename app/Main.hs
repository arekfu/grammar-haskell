module Main where

import Grammar
import Grammar.CFG
import Grammar.MC

import System.Clock

exampleGrammarString :: String
exampleGrammarString = "\
\ 'E' := 'E' '+' 'E' | 'E' '*' 'E' | '(' 'E' ')' | 'I'\n\
\ 'I' := 'a' | 'b' | 'c' | 'I' 'a' | 'I' 'b' | 'I' 'c'"

exampleGrammar :: CharCFG
exampleGrammar = parseCharCFG (Quoting '\'' '\'') 'E' exampleGrammarString

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
          mapM_ (putStrLn . concatMap (quote NoQuoting)) strings
          putStrLn ""
          -- digits grammar
          putStrLn $ showGrammar digitsGrammar
          let digitExpansion = randomGrammarDerive digitsGrammar
          putStrLn $ concatMap (quote NoQuoting) $ evalMC digitExpansion $ fromIntegral seed
          putStrLn ""
          -- examine the structureof the regexes
          let (Just rs) = productions digitsGrammar (ReprString "<D>")
          putStrLn $ showRegex rs
