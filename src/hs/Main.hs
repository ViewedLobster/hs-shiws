-- TODO implement socket handling

import Control.Concurrent.MVar
import Network.Socket
import Data.ByteString
import EPoll (EPoll, EPollEvt (..), EPollOpt (..), create, register, unregister, wait)

newtype RdySignal = RdySignal (MVar (Bool, Maybe (MVar ())))

-- Should signal be ready on creation?
newRdySignal = RdySignal <$> (newMVar (False, Nothing))

data WrappedSocket = AcceptSocket Socket
                   | AsyncSocket Socket RdySignal RdySignal

wrapSocket :: EPoll WrappedSocket -> Socket -> IO WrappedSocket
wrapSocket epoll sock = do
    r <- newRdySignal
    w <- newRdySignal
    let as = AsyncSocket sock r w 
        sfd = fdSocket sock in
        do register epoll sfd [EPOLLIN, EPOLLOUT, EPOLLHUP, EPOLLRDHUP, EPOLLERR, EPOLLET, EPOLLPRI] as
           return as

wrapAccept :: EPoll WrappedSocket -> Socket -> IO WrappedSocket
wrapAccept epoll sock = do
    let as = AcceptSocket sock
        sfd = fdSocket sock in
        do register epoll sfd [EPOLLIN, EPOLLERR, EPOLLET] as
           return as
            

killSocket :: EPoll WrappedSocket -> WrappedSocket -> IO ()
killSocket epoll wrapped = do
    let sfd = case wrapped of
                  AcceptSocket sock    -> fdSocket sock
                  AsyncSocket sock _ _ -> fdSocket sock in
        do unregister epoll sfd
           close sock

asyncRecv :: WrappedSocket -> Int -> IO ByteString
asyncRecv (AsyncSocket sock rdyRd _) num = do
    waitRdy rdyRd
    recv


asyncSend :: WrappedSocket -> ByteString -> IO ()
asyncSend = undefined

handleRequest :: WrappedSocket -> IO ()
handleRequest sock = do
    bs <- asyncRecv sock 1024
    if length bs > 0 then do
        asyncSend sock bs
        handleRequest sock
    else killSocket sock
    


main = do
    putStrLn "Starting server"
    epoll <- create
    asock <- socket AF_INET Stream 0
    bind asock $ SockAddrInet 1234 $ tupleToHostAddress (127, 0, 0, 1)
    listen asock 64
    wrapped <- wrapAccept asock
    
    -- start accepting
