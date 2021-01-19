-- from https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html
module Server (
  runServerGame
) where
    
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import TicTacToe ( runGame, Cell, Res(..), InputGetter, ParseVerifier )
import Text.ParserCombinators.Parsec ( ParseError ) 
import Parsers ( isexit, parsePairInt )
import Util ( str2bs, bs2str, r2j, handle )

runServerGame :: IO ()
runServerGame = runTCPServer Nothing "3000" rg
  where rg sock = runGame $ getMoveServer sock

gtFromClient :: Socket -> IO String 
gtFromClient sock = do
  msg <- recv sock 1024
  return $ bs2str msg

getMoveServer :: Socket -> Res -> InputGetter
getMoveServer sock r verify 
  | r == None = do
      putStr "Try again: " 
      handle ig (Just sock) verify getLine
  | r == O = do
      putStr "Your move: "
      handle ig (Just sock) verify getLine 
  | r == X = do
      putStr "Opponent's move...\n"
      handle ig Nothing verify (gtFromClient sock) 
    where 
      ig = getMoveServer sock None

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
    where
      resolve = do
        let hints = defaultHints {
          addrFlags = [AI_PASSIVE],
          addrSocketType = Stream
        }
        head <$> getAddrInfo (Just hints) mhost (Just port)
      open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
      loop sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)