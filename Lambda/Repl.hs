module Repl (repl) where

import System.IO (hFlush, stdout)

-- This function creates a read-eval-print loop that applies a
-- function to each line of input. If the function returns `Nothing`,
-- the REPL is halted; otherwise, it prints the result and continues.
repl :: (String -> Maybe String) -> IO ()
repl f = putStr "> " >>
         hFlush stdout >>
         getLine >>= \l ->
         case f l of
           Just s  -> putStrLn s >> repl f
           Nothing -> return ()
