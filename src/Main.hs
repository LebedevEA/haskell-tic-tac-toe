module Main where

import TicTacToe
import Text.ParserCombinators.Parsec

int :: Parser Int 
int = do 
  s <- many1 digit 
  return $ read s 

pairint :: Parser Cell
pairint = do
  x <- spaces >> int 
  y <- spaces >> int
  return (x,y)

parsePairInt :: String -> Either ParseError Cell
parsePairInt = parse pairint ""

getMove :: (Either ParseError Cell -> IO Bool) -> IO Cell
getMove verify = do 
  line <- getLine
  v <- verify (parsePairInt line)
  return undefined 

main :: IO ()
main = runGame getMove putStr

