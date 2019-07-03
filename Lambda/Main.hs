module Main where

import Lambda.Parser (parseTerm)
import Lambda.Reduction (reduce)
import Lambda.Repl (repl)
import Lambda.Term (Term(..), removeNames, restoreNames)

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
