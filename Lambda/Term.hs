module Term (Term(..)) where

data Term = Var String
          | Abs String Term
          | App Term Term

instance Show Term where
  show (Var x)   = x
  show (Abs n b) = "(\\" ++ n ++ " -> " ++ show b ++ ")"
  show (App t u) = show t ++ " " ++ show u
