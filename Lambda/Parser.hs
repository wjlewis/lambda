module Parser where

import Data.Char (isSpace, isLower)
import Parsing
import Term (Term(..))
import Prelude hiding (abs)
import Result (Result(..))

parseTerm :: String -> Either String Term
parseTerm inp = case runParser p inp of
                  Ok (t,_) -> Right t
                  Miss m   -> Left m
                  Fail m   -> Left m
  where p = parse term <* eof

term :: Parser Term
term = term' `chainl1` pure App
  where term' = abs <|> var <|> parenTerm

abs :: Parser Term
abs = try binder >>
      param      >>= \n ->
      arrow      >>
      body       >>= \b ->
      return $ Abs n b
  where
    binder = token $ char '\\'
    param  = failWith "expected parameter" $ token name
    arrow  = token $ string "->"
    body   = failWith "expected lambda body" term

name :: Parser String
name = failWith "illegal name" $ many1 $ sat isLower

var :: Parser Term
var = try (Var <$> token name)

parenTerm :: Parser Term
parenTerm = open *> term <* close
  where
    open  = failWith "invalid lambda term" (token $ char '(')
    close = failWith "missing ')'" (token $ char ')')

-- These parsers are all used to to simplify the handling of
-- whitespace between meaningful characters. `parse` should be used at
-- the "top level" to remove all leading insignificant characters, and
-- `token` should be applied to each meaningful parser to advance the
-- input to the next significant character.
space :: Parser Char
space = sat isSpace

junk :: Parser ()
junk = many space *> pure ()

parse :: Parser a -> Parser a
parse p = junk *> p

token :: Parser a -> Parser a
token p = p <* junk
