module Parsers where

import Text.ParserCombinators.Parsec
    ( alphaNum, digit, spaces, many1, parse, ParseError, Parser ) 
import TicTacToe (Cell)

int :: Parser Int 
int = do 
  s <- many1 digit 
  return $ read s 

pairint :: Parser Cell
pairint = do
  x <- spaces >> int 
  y <- spaces >> int
  return (x,y)

word :: Parser String 
word = spaces >> many1 alphaNum 

isexit :: String -> Bool 
isexit s
  | s == "exit" = True 
  | otherwise = False

parsePairInt :: String -> Either ParseError Cell
parsePairInt = parse pairint ""