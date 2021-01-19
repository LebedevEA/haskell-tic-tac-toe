module Util where

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import TicTacToe ( InputGetter, ParseVerifier, Cell )
import Network.Socket ( Socket ) 
import Parsers ( isexit, parsePairInt )
import Network.Socket.ByteString ( sendAll )

str2bs :: String -> C.ByteString
str2bs = encodeUtf8 . T.pack

bs2str :: C.ByteString -> String 
bs2str = read . show

r2j :: Either a b -> Maybe b
r2j (Right b) = Just b

handle :: InputGetter -> Maybe Socket -> ParseVerifier -> IO String -> IO (Maybe Cell)
handle ig sock verify gtLn = do
  line <- gtLn 
  if isexit line
  then do
    putStrLn "Bye!"
    case sock of
      Just s -> sendAll s $ str2bs line
      Nothing -> return ()
    return Nothing 
  else do
    let parsed = parsePairInt line
    let v =verify parsed
    case v of
      Just err -> do
        putStr err 
        ig verify
      Nothing -> do
        case sock of
            Just s -> sendAll s $ str2bs line
            Nothing -> return ()
        return $ r2j parsed