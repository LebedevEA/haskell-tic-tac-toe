module Main where

import TicTacToe
import Text.ParserCombinators.Parsec

int :: Parser Int 
int = do 
  s <- many1 digit 
  return $ read s 

pairint :: Parser (Int, Int)
pairint = do
  x <- spaces >> int 
  y <- spaces >> int
  return (x,y)

parsePairInt :: String -> Either ParseError (Int,Int)
parsePairInt = parse pairint ""

getMove :: IO (Int, Int) 
getMove = do 
  line <- getLine
  case parsePairInt line of
    Right p -> return p
    Left _ -> undefined

main :: IO ()
main = runGame getMove putStr

