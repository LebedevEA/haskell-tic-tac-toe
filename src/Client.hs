-- from https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html
module Client where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Network.Socket 
import Network.Socket.ByteString (recv, sendAll)
import Data.Text.Encoding (encodeUtf8)
import TicTacToe ( Cell, Res(..) )
import Text.ParserCombinators.Parsec ( ParseError ) 
import Parsers ( isexit, parsePairInt )

main :: IO ()
main = runTCPClient "127.0.0.1" "3000" $ \s -> do
    sendAll s $ encodeUtf8 $ T.pack "Hello, from client!"
    msg <- recv s 1024
    putStr "Received: "
    C.putStrLn msg

-- getMoveOffline :: Res -> (String -> IO ()) -> (Either ParseError Cell -> Maybe String) -> IO (Maybe Cell)
-- getMoveOffline plyr prnt verify = do 
--   prntPlyrMv prnt plyr
--   line <- getLine
--   if isexit line
--   then do
--     prnt "Bye!\n"
--     return Nothing 
--   else do
--     let parsed = parsePairInt line
--     let v = verify parsed
--     case v of
--       Just error -> do
--         prnt error
--         getMoveOffline plyr prnt verify
--       Nothing -> return $ r2j parsed
--         where 
--           r2j (Right b) = Just b

--- Client always has second (as X) move

str2bs :: String -> C.ByteString
str2bs = encodeUtf8 . T.pack

bs2str :: C.ByteString -> String 
bs2str = read . show -- may not work :(

pr2bs :: (Int,Int) -> C.ByteString
pr2bs = str2bs . show

bs2pr :: C.ByteString -> (Int,Int)
bs2pr = read . bs2str



getMoveClient :: Socket -> Res -> (String -> IO ()) -> (Either ParseError Cell -> Maybe String) -> IO (Maybe Cell)
getMoveClient sock O 
  


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