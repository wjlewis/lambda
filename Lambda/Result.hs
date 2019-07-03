module Lambda.Result (Result(..)) where

import Control.Monad (ap, liftM)

-- This is the return type for parsers. The `Ok` variant is used to
-- indicate a successful parse, `Miss` for an unsuccessful *but
-- continuable* parse, and `Fail` for an uncontinuable parse.
data Result a = Ok a
              | Miss String
              | Fail String
  deriving Show

instance Functor Result where
  fmap = liftM

instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  return = Ok
  r >>= f = case r of
              Ok x   -> f x
              Miss m -> Miss m
              Fail m -> Fail m
