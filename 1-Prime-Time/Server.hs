{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent (forkIO)
import Data.Aeson
import qualified Data.ByteString.UTF8 as S
import qualified Data.ByteString.Lazy.UTF8 as SL
import GHC.Generics
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (getArgs)
import System.IO

import Prime (isPrime)

data Request = Request {
      method :: String
    , number :: Double
    } deriving (Generic, Show)

instance ToJSON Request where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Request

data Response = Response {
      method :: String
    , prime :: Bool
    } deriving (Eq, Generic, Show)

instance ToJSON Response where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Response

main :: IO ()
main = do
    let hostAddr = tupleToHostAddress (0, 0, 0, 0)
    port <- getPort

    sock <- socket AF_INET Stream defaultProtocol
    bind sock $ SockAddrInet port hostAddr
    listen sock 5
    putStrLn $ "Listening on " ++ show hostAddr ++ ":" ++ show port

    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    forkIO (runConn conn)
    mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, addr) = do
    putStrLn $ "Got connection from " ++ show addr
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    serveWithHandle hdl

serveWithHandle :: Handle -> IO ()
serveWithHandle hdl = loop where
    loop = do
        line <- hGetLine hdl
        let mrequest = decode (SL.fromString line) :: Maybe Request
        let response = getResponse mrequest
        hPutStrLn hdl $ SL.toString $ encode response
        if response == malformedResponse
           then hClose hdl
           else loop

getResponse :: Maybe Request -> Response
getResponse Nothing = malformedResponse
getResponse (Just (Request { method = "isPrime", number = n }))
  | ceiling n == floor n = Response { method = "isPrime", prime = isPrime (ceiling n) }
  | otherwise = malformedResponse
getResponse _ = malformedResponse

malformedResponse :: Response
malformedResponse = Response {method = "notIsPrime", prime = False}

getPort :: IO PortNumber
getPort = do
    args <- getArgs
    let port = read (head args) :: PortNumber
    return port

recvAll :: Socket ->IO S.ByteString
recvAll sock = do
    -- TODO: maybe actually recv all?
    msg <- recv sock 1024
    return msg
