module Main where

import Server ( runServerGame )
import Client ( runClientGame )
import Offline ( runOfflineGame )
import Parsers ( parseWord )
import System.IO ( hSetBuffering, stdout, BufferMode( NoBuffering ) )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering 
  putStr "Game mode: " 
  line <- getLine 
  case parseWord line of
    Right "offline" -> runOfflineGame 
    Right "server" -> runServerGame 
    Right "client" -> runClientGame
    _ -> putStrLn "Parse error!" >> main


