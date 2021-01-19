module Main where

import TicTacToe ( runGame, Cell, Res(..) )

import Text.ParserCombinators.Parsec ( ParseError ) 
import Parsers ( isexit, parsePairInt )

prntPlyrMv :: (String -> IO ()) -> Res -> IO ()
prntPlyrMv _ None = return ()
prntPlyrMv prnt plyr = prnt $ show plyr ++ "'s move! "

getMoveOffline :: Res -> (String -> IO ()) -> (Either ParseError Cell -> Maybe String) -> IO (Maybe Cell)
getMoveOffline plyr prnt verify = do 
  prntPlyrMv prnt plyr
  line <- getLine
  if isexit line
  then do
    prnt "Bye!\n"
    return Nothing 
  else do
    let parsed = parsePairInt line
    let v = verify parsed
    case v of
      Just error -> do
        prnt error
        getMoveOffline plyr prnt verify
      Nothing -> return $ r2j parsed
        where 
          r2j (Right b) = Just b

main :: IO ()
main = runGame getMoveOffline putStr

