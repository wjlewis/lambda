module Main where

import Parser (parseTerm)
import Reduction (reduce)
import Repl (repl)
import Term (Term(..), removeNames, restoreNames)

main :: IO ()
main = putStrLn "λ-repl\n:q to quit\n" >>
       repl process >>
       putStrLn "goodbye"

process :: String -> Maybe String
process inp
  | inp == ":q" = Nothing
  | otherwise   = case parseTerm inp of
                    Left m  -> Just $ "Parser error: " ++ m
                    Right t -> Just $ normalizeTerm t

normalizeTerm :: Term -> String
normalizeTerm = show
              . restoreNames
              . reduce
              . removeNames
