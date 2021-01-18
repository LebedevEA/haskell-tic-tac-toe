module Main where

import TicTacToe ( runGame, Cell )

import Text.ParserCombinators.Parsec ( ParseError ) 
import Parsers ( isexit, parsePairInt )

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

