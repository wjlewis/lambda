module Parsing
( Parser(..)
, (<|>)
, chainl1
, char
, eof
, fail
, failWith
, many
, many1
, next
, sat
, string
, try
) where

import Control.Monad (ap, liftM)
import Prelude hiding (fail)
import Result (Result(..))

-- This Parser type is directly inspired by Hutton and Meijer's
-- exposition on parser combinators. I've used `Result` as the return
-- type in order to distinguish between 2 different failure
-- variations: continuable failure (a "miss") and terminal failure (a
-- "fail"). This allows for more efficient parsing and better error
-- messages. For more information, see the functional pearl "Monadic
-- Parser Combinators" by the authors mentioned above.
newtype Parser a = Parser { runParser :: String -> Result (a, String) }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return x = Parser $ \inp -> return (x, inp)
  p >>= f = Parser $ \inp -> runParser p inp >>= \(x, inp') ->
                             runParser (f x) inp'

-- This parser simply consumes the next available character, or fails
-- if none is available. I've chosen to have all parsers return
-- terminal failures by default; this should speed up parsing and
-- allow for better error messages. That said, I've provided a way to
-- convert a parser that fails to a parser that merely misses with the
-- `try` combinator below.
next :: Parser Char
next = Parser f
  where
    f ""     = Fail "unexpected EOF"
    f (c:cs) = pure (c, cs)

-- `eof` complements `next` in that it succeeds if the input string is
-- empty, and fails otherwise.
eof :: Parser ()
eof = Parser f
  where
    f "" = pure ((), "")
    f _  = Fail "expected EOF"

fail :: String -> Parser a
fail m = Parser $ \_ -> Fail m

-- This choice combinator constructs a parser that first tries the
-- left parser on its input; if this parser succeeds, the composite
-- parser succeeds with this value; if it misses, the composite parser
-- tries the right parser; if it fails, the composite parser fails. It
-- is for this reason that we have included not one but *two* failure
-- variations.
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser $ \inp -> case runParser p inp of
  Miss _ -> runParser q inp
  r      -> r

-- In many cases, we'd like to provide a specific error message when a
-- parser fails. This parser allows you to do just that: in the event
-- of a failure, it uses the provided message, but does not alter the
-- given parser's behavior otherwise.
failWith :: String -> Parser a -> Parser a
failWith m p = Parser $ \inp -> case runParser p inp of
                                  Fail _ -> Fail m
                                  r      -> r

-- I alluded to this combinator above, which converts a failing parser
-- into a parser that misses. This allows for backtracking in the
-- event of failure, and is used exclusively with the choice
-- combinator (<|>).
try :: Parser a -> Parser a
try p = Parser $ \inp -> case runParser p inp of
                           Fail m -> Miss m
                           r      -> r

-- As its name indicates, `many` constructs a parser that applies the
-- provided parser zero or more times until it fails. It collects the
-- results in a list. This combinator also includes a use of the `try`
-- combinator defined above.
many :: Parser a -> Parser [a]
many p = try more <|> done
  where
    more = (:) <$> p <*> many p
    done = pure []

-- This is almost exactly like `many` above, except that the provided parser *must* succeed at least once.
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- This combinator is invaluable in parsing repeated terms with some
-- meaningful separator. It takes two parsers, the first of which
-- should parse a term, and the second of which should parse a
-- separator and produce a function for combining terms. The composite
-- parser then folds the resulting terms together using the separator
-- functions. See the Hutton and Meijer paper for a more detailed
-- treatment.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p         >>= \z  ->
                 many pair >>= \ps ->
                 return $ foldl f z ps
  where
    pair        = (,) <$> op <*> p
    f z (op, x) = z `op` x

-- This combinator produces a parser that succeeds or fails depending
-- on whether the character it consumes satisfies the provided
-- predicate.
sat :: (Char -> Bool) -> Parser Char
sat p = next >>= \x -> if p x then return x else fail m
  where m = "failed to satisfy predicate"

-- Here we use the `sat` combinator to require that the given input
-- starts with a certain character. I've also used the `failWith`
-- combinator to provide a more informative error message in the event
-- of failure.
char :: Char -> Parser Char
char c = failWith m $ sat (==c)
  where m = "expected " ++ show c

-- This is similar to the `char` combinator above, except that it
-- parses strings instead.
string :: String -> Parser String
string s = f s
  where
    f ""     = pure ""
    f (c:cs) = (:) <$> failWith m (char c) <*> f cs
    m = "expected \"" ++ s ++ "\""
