-- from https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html
module Server (
  runTCPServer
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
import TicTacToe ( runGame, Cell, Res(..) )
import Text.ParserCombinators.Parsec ( ParseError ) 
import Parsers ( isexit, parsePairInt )

main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    talk s = do
        msg <- recv s 1024
        unless (S.null msg) $ do
          C.putStrLn msg
          sendAll s $ encodeUtf8 $ T.pack "Hello, from server!"
          talk s

runServerGame :: IO ()
runServerGame = runTCPServer Nothing "3000" rg
  where rg sock = runGame (getMoveServer sock) putStr


gtFromClient :: Socket -> IO String 
gtFromClient sock = do
  msg <- recv sock 1024
  return $ bs2str msg

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
    print "Bye!\n"
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
        getMoveServer sock None prnt verify
      Nothing -> do
        if doSend 
        then sendAll sock $ str2bs line
        else do return ()
        return $ r2j parsed

getMoveServer :: Socket -> Res -> (String -> IO ()) -> (Either ParseError Cell -> Maybe String) -> IO (Maybe Cell)
getMoveServer sock None prnt verify = do
  prnt "Try again: " 
  handle sock prnt verify getLine True
getMoveServer sock O prnt verify = do
  prnt "Opponent's move...\n"
  handle sock prnt verify (gtFromClient sock) False
getMoveServer sock X prnt verify = do
  prnt "Your move: "
  handle sock prnt verify getLine True

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