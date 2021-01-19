-- from https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html
module Client where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Network.Socket 
import Network.Socket.ByteString (recv, sendAll)
import Data.Text.Encoding (encodeUtf8)
import TicTacToe ( Cell )
import Text.ParserCombinators.Parsec ( ParseError ) 
import Parsers ( isexit, parsePairInt )

main :: IO ()
main = runTCPClient "127.0.0.1" "3000" $ \s -> do
    sendAll s $ encodeUtf8 $ T.pack "Hello, from client!"
    msg <- recv s 1024
    putStr "Received: "
    C.putStrLn msg

-- getMove :: (Either ParseError Cell -> IO Bool) -> IO (Maybe Cell)
-- getMove verify = do 
--   line <- getLine
--   if isexit line
--   then return Nothing 
--   else do
--     v <- verify (parsePairInt line)
--     if v
--     then return $ r2j $ parsePairInt line
--     else getMove verify
--       where 
--         r2j (Right b) = Just b

--- Client always has second (as X) move

str2bs :: String -> C.ByteString
str2bs = encodeUtf8 . T.pack

bs2str :: C.ByteString -> String 
bs2str = read . show -- may not work :(

pr2bs :: (Int,Int) -> C.ByteString
pr2bs = str2bs . show

bs2pr :: C.ByteString -> (Int,Int)
bs2pr = read . bs2str



getMove :: Socket -> (Either ParseError Cell -> IO Bool) -> IO (Maybe Cell)
getMove sock = do undefined 
  


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