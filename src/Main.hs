module Main where

import TicTacToe ( runGame, Cell )
import Text.ParserCombinators.Parsec
    ( alphaNum, digit, spaces, many1, parse, ParseError, Parser )
import Data.Either (fromRight)

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

getMove :: (Either ParseError Cell -> IO Bool) -> IO (Maybe Cell)
getMove verify = do 
  line <- getLine
  if isexit line
  then return Nothing 
  else do
    v <- verify (parsePairInt line)
    if v
    then return $ r2j $ parsePairInt line
    else getMove verify
      where 
        r2j (Right b) = Just b

main :: IO ()
main = runGame getMove putStr

