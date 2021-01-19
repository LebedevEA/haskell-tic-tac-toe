-- from https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html
module Client where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Network.Socket 
import Network.Socket.ByteString (recv, sendAll)
import Data.Text.Encoding (encodeUtf8)
import TicTacToe ( runGame, Cell, Res(..) )
import Text.ParserCombinators.Parsec ( ParseError ) 
import Parsers ( isexit, parsePairInt )

runClientGame :: IO ()
runClientGame = runTCPClient "127.0.0.1" "3000" rg
  where rg sock = runGame (getMoveClient sock) putStr

gtFromServer :: Socket -> IO String 
gtFromServer sock = do
  msg <- recv sock 1024
  return $ bs2str msg

--- Client always has second (as X) move

str2bs :: String -> C.ByteString
str2bs = encodeUtf8 . T.pack

bs2str :: C.ByteString -> String 
bs2str = read . show -- may not work :(

r2j :: Either a b -> Maybe b
r2j (Right b) = Just b

handle :: Socket -> (String -> IO ()) -> (Either ParseError Cell -> Maybe String) -> IO String -> Bool -> IO (Maybe Cell)
handle sock prnt verify gtLn doSend = do
  line <- gtLn 
  if isexit line
  then do
    prnt "Bye!\n"
    if doSend 
    then sendAll sock $ str2bs line
    else do return ()
    return Nothing 
  else do
    let parsed = parsePairInt line
    let v =verify parsed
    case v of
      Just error -> do
        prnt error
        getMoveClient sock None prnt verify
      Nothing -> do
        if doSend 
        then sendAll sock $ str2bs line
        else do return ()
        return $ r2j parsed

getMoveClient :: Socket -> Res -> (String -> IO ()) -> (Either ParseError Cell -> Maybe String) -> IO (Maybe Cell)
getMoveClient sock None prnt verify = do
  prnt "Try again: " 
  handle sock prnt verify getLine True
getMoveClient sock O prnt verify = do
  prnt "Your move: "
  handle sock prnt verify getLine True 
getMoveClient sock X prnt verify = do
  prnt "Opponent's move...\n"
  handle sock prnt verify (gtFromServer sock) False 

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
      let hints = defaultHints { addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock