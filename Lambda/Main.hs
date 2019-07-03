module Main where

import Repl (repl)

main :: IO ()
main = putStrLn "λ-repl\n:h for help, :q to quit\n" >>
       repl process >>
       putStrLn "goodbye"

process :: String -> Maybe String
process inp
  | inp == ":q" = Nothing
  | inp == ":h" = Just "<help info>"
  | otherwise   = Just inp
