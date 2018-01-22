module Arithm where

import Text.Parsec
import Text.Parsec.Expr
import Data.Char (digitToInt)
import Text.Parsec.Expr


-- data Info = Info { row :: Int, col :: Int } deriving (Show)
-- infoFrom :: SourcePos -> Info
-- infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

data TermOps a = TermConstruction
      { constInt:: Int -> a
      , plus    :: a -> a -> a
      , mul     :: a -> a -> a
      }

parsingOps :: TermOps a
parsingOps = undefined

eval :: a -> Int
eval _ = 42 -- implement this function

stringConst :: String -> Parsec String u ()
stringConst [] = return ()
stringConst (c:cs) = do
  char c
  stringConst cs

-- parsing stuff
decimal :: TermOps a -> Parsec String () a
decimal ops = do
  digits <- many1 digit
  let n = foldl (\acc d -> 10*acc + digitToInt d) 0 digits
  seq n (return $ constInt ops n)


parser :: TermOps a -> Parsec String () a
parser ops = buildExpressionParser (table ops) (spaces *> (decimal ops) <* spaces)
              <?> "aithmetic expression"
  where
    table ops =
      [ [binary "*" (mul  ops) AssocLeft ]
      , [binary "+" (plus ops) AssocLeft ]
      ]

    binary name f assoc = Infix (do{ stringConst name; return f }) assoc

parse :: TermOps a -> String -> Either ParseError a
parse ops = runParser (parser ops) () "untyped λ-calculus"
