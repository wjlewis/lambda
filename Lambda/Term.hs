module Term (Term(..)) where

import Data.List (intercalate)

data Term = Var String
          | Abs String Term
          | App Term Term

instance Show Term where
  show (Var x)   = x

  -- We parenthesize a subterm in an application in 2 cases: when the
  -- first subterm is an abstraction (since the bodies of abstractions
  -- extend as far to the right as possible), and when the second
  -- subterm is an application (since application is left-associative
  -- by default).
  show (App t@(Abs _ _) u) = "(" ++ show t ++ ") " ++ show u
  show (App t u@(App _ _)) = show t ++ " (" ++ show u ++ ")"
  show (App t u)           = show t ++ " " ++ show u

  -- Here we make use of the convention that abstractions with
  -- multiple bound variables represent nested/curried abstractions in
  -- order to show them in a compact format.
  show (Abs n b) = "\\" ++ names ++ " -> " ++ show body
    where
      names = intercalate " " $ (n : collectNames b)
      body  = findBody b
      collectNames t = case t of
                         (Abs n' b') -> n' : collectNames b'
                         t           -> []
      findBody t     = case t of
                         (Abs _ b') -> findBody b'
                         t          -> t
