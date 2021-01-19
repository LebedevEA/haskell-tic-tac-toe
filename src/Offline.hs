module Offline (
  runOfflineGame
) where

import TicTacToe ( runGame, Res(..), InputGetter )
import Parsers ( isexit )
import Util ( handle )


runOfflineGame :: IO ()
runOfflineGame = runGame getMoveOffline

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