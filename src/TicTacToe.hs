{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module TicTacToe (
  runGame,
  Res(..)
) where

import Control.Lens

data Res = None 
          | X 
          | O 
          deriving Eq 

instance Show Res where 
  show :: Res -> String 
  show r | r == X = "X" 
         | r == O = "O" 
         | r == None = "." 

data Row = Row Res Res Res
         deriving Eq

instance Show Row where
  show (Row a b c) = 
    show a ++ " " ++
    show b ++ " " ++
    show c

data Board = Board Row Row Row
           deriving Eq

instance Show Board where
  show (Board a b c) =
    show a ++ "\n" ++
    show b ++ "\n" ++
    show c ++ "\n"

mkRow :: Row
mkRow = Row None None None

mkBoard :: Board
mkBoard = Board mkRow mkRow mkRow

instance Field1 Board Board Row Row where
  _1 :: Lens Board Board Row Row
  _1 = lens get set
    where 
      get (Board a _ _) = a
      set (Board _ b c) q = Board q b c

instance Field2 Board Board Row Row where
  _2 :: Lens Board Board Row Row
  _2 = lens get set
    where 
      get (Board _ b _) = b
      set (Board a _ c) q = Board a q c

instance Field3 Board Board Row Row where
  _3 :: Lens Board Board Row Row
  _3 = lens get set
    where 
      get (Board _ _ c) = c
      set (Board a b _) q = Board a b q

instance Field1 Row Row Res Res where
  _1 :: Lens Row Row Res Res
  _1 = lens get set
    where 
      get (Row a _ _) = a
      set (Row _ b c) q = Row q b c

instance Field2 Row Row Res Res where
  _2 :: Lens Row Row Res Res
  _2 = lens get set
    where 
      get (Row _ b _) = b
      set (Row a _ c) q = Row a q c

instance Field3 Row Row Res Res where
  _3 :: Lens Row Row Res Res
  _3 = lens get set
    where 
      get (Row _ _ c) = c
      set (Row a b _) q = Row a b q

ntl :: (Field1 a a b b, Field2 a a b b, Field3 a a b b) 
         => Int -> Lens a a b b
ntl n 
  | n == 0 = _1
  | n == 1 = _2
  | n == 2 = _3

mkHorWinRow :: (Num b, Enum b) => a -> [(a, b)]
mkHorWinRow n = zip (repeat n) [0..2]

mkVertWinRow :: (Num b, Enum b) => a -> [(b, a)]
mkVertWinRow n = zip  [0..2] (repeat n)

mkDiagWinRow :: (Num b, Enum b) => Bool -> [(b, b)]
mkDiagWinRow b 
  | b = zip [0..2] [0..2]
  | otherwise = zip [2,1,0] [0..2]

winRows :: [[(Int,Int)]]
winRows = map mkHorWinRow [0..2] ++ 
          map mkVertWinRow [0..2] ++
          map mkDiagWinRow [True, False]

allEqual :: Eq a => [a] -> Bool 
allEqual (x:xs) = all (== x) xs

testWinRow :: Board -> [(Int,Int)] -> Res
testWinRow board pos =
  if allEqual $ map (get board) pos
  then get board (head pos)
  else None

get :: Board -> (Int,Int) -> Res
get board (x,y) = board ^. ntl x . ntl y

testWin :: Board -> Res
testWin board
  | O `elem` res = O
  | X `elem` res = X
  | otherwise = None
    where 
      res = map (testWinRow board) winRows

move :: Board -> Int -> Int -> Res -> Board
move board x y res 
  | res == None = error "move: set None"
  | otherwise =
    case board ^. ntl x . ntl y of
      None -> board & ntl x . ntl y .~ res
      _ -> error "move: set not on None"

runGame :: IO (Int,Int) -> (String -> IO ()) -> IO ()
runGame getMove print = rg mkBoard getMove print O

rg :: Board -> IO (Int,Int) -> (String -> IO ()) -> Res -> IO () 
rg board getMove prnt plyr = do 
  prnt $ show board
  prnt $ show plyr ++ "'s move:"
  (x,y) <- getMove 
  prnt "\n"
  let board' = move board x y plyr
  case testWin board' of 
    O -> endGame board' O prnt
    X -> endGame board' X prnt
    None -> 
      if canMove board'
      then rg board' getMove prnt $ nxtplyr plyr 
      else endGame board' None prnt

verify :: Board -> IO () -> (Int, Int) -> IO Bool
verify board errmsg (x,y)
  | x < 0 || x > 2 = fail
  | y < 0 || y > 2 = fail
  | get board (x,y) /= None = fail
  | otherwise = return True
    where
      fail = errmsg >> return False 


  -- | x < 0 || x > 3 || y < 0 || y > 3 
  --     || get board (x,y) /= None = do
  --       errmsg
--   --       return False

-- inRange n (x,y)
--   | x <= n && n <= y = True 
--   | otherwise = False

canMove :: Board -> Bool 
canMove board = None `elem` elems
  where 
    elems = map (get board) $ (,) <$> [0..2] <*> [0..2]

endGame :: Board -> Res -> (String -> IO ()) -> IO ()
endGame board res prnt = do
  prnt $ show board
  prnt $ showGameRes res

showGameRes :: Res -> String 
showGameRes res 
  | res == None = "Draw!\n"
  | otherwise = show res ++ " wins!\n"

nxtplyr :: Res -> Res 
nxtplyr p 
  | p == X = O 
  | p == O = X 