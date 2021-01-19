-- from https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html
module Client (
  runClientGame
) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Network.Socket 
import Network.Socket.ByteString (recv, sendAll)
import Data.Text.Encoding (encodeUtf8)
import TicTacToe ( runGame, Cell, Res(..), InputGetter, ParseVerifier )
import Text.ParserCombinators.Parsec ( ParseError ) 
import Parsers ( isexit, parsePairInt )
import Util ( str2bs, bs2str, r2j, handle )

runClientGame :: IO ()
runClientGame = runTCPClient "127.0.0.1" "3000" rg
  where rg sock = runGame $ getMoveClient sock

gtFromServer :: Socket -> IO String 
gtFromServer sock = do
  msg <- recv sock 1024
  return $ bs2str msg

getMoveClient :: Socket -> Res -> InputGetter
getMoveClient sock r verify 
  | r == None = do
      putStr "Try again: " 
      handle ig (Just sock) verify getLine
  | r == X = do
      putStr "Your move: "
      handle ig (Just sock) verify getLine 
  | r == O = do
      putStr "Opponent's move...\n"
      handle ig Nothing verify (gtFromServer sock) 
    where 
      ig = getMoveClient sock None

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
      let hints = defaultHints { 
        addrSocketType = Stream 
      }
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock