import Control.Concurrent (forkIO)
import Network.Socket
import qualified Data.ByteString as B
import System.Environment (getArgs)
import System.IO (hClose, hIsEOF, hSetBuffering, BufferMode(..), Handle, IOMode(ReadWriteMode))

main :: IO ()
main = do
    args <- getArgs
    let port = read (head args) :: PortNumber
    let hostAddr = tupleToHostAddress (0, 0, 0, 0)

    sock <- socket AF_INET Stream defaultProtocol
    bind sock $ SockAddrInet port hostAddr
    listen sock 5
    putStrLn $ "Listening on port " ++ (show port)

    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    forkIO (runConn conn)
    mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, addr) = do
    putStrLn $ "Got connection from " ++ show addr
    handle <- socketToHandle sock ReadWriteMode 
    hSetBuffering handle NoBuffering
    echoFromHandle handle
    putStrLn $ "closing sock for " ++ show addr
    close sock

echoFromHandle :: Handle -> IO ()
echoFromHandle handle = do
    done <- hIsEOF handle
    if done
       then do
           putStrLn "got EOF"
           hClose handle
    else do
        some <- B.hGetNonBlocking handle 1024
        B.hPut handle some
        echoFromHandle handle
