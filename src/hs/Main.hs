-- TODO implement IO system
-- TODO set up EPoll

import Control.Concurrent.MVar
import Network.Socket
import EPoll (EPoll, EPollEvt (..), EPollOpt (..), create, register, unregister)

newtype RdySignal = RdySignal (MVar (Bool, Maybe (MVar ())))

-- Should signal be ready on creation?
newRdySignal = RdySignal <$> (newMVar (False, Nothing))

data AsyncSocket = AsyncSocket
    {
    sock :: Socket,
    rdyRead :: RdySignal,
    rdyWrite :: RdySignal
    }

wrapSocket :: EPoll AsyncSocket -> Socket -> IO AsyncSocket
wrapSocket epoll sock = do
    r <- newRdySignal
    w <- newRdySignal
    let as = AsyncSocket sock r w 
        sfd = fdSocket sock in do
        register epoll sfd [EPOLLIN, EPOLLOUT, EPOLLHUP, EPOLLRDHUP, EPOLLERR, EPOLLET, EPOLLPRI] as
        return as

killSocket :: EPoll AsyncSocket -> AsyncSocket -> IO ()
killSocket epoll (AsyncSocket sock rd wr) = do
    let sfd = fdSocket sock in do
        unregister epoll sfd
        close sock

main = do
    putStrLn "Starting server"
    epoll <- create
    asock <- socket AF_INET Stream 0
    bind asock $ SockAddrInet 1234 $ tupleToHostAddress (127, 0, 0, 1)
    listen asock 64
    -- register to epoll
    -- start accepting
