module Main where

import TicTacToe ( runGame, Cell, Res(..), InputGetter )

import Text.ParserCombinators.Parsec ( ParseError ) 
import Parsers ( isexit, parsePairInt )
import Util ( r2j, handle )

getMoveOffline :: Res -> InputGetter
getMoveOffline r verify 
  | r == None = do
      putStr "Try again: " 
      handle ig Nothing verify getLine
  | r == X = do
      putStr "X's move: "
      handle ig Nothing verify getLine 
  | r == O = do
      putStr "O's move: "
      handle ig Nothing verify getLine
    where 
      ig = getMoveOffline None

main :: IO ()
main = runGame getMoveOffline

