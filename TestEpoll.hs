
import EPoll
import Network.Socket
import Foreign.C.Types

data SocketType = Listen
                | Comm



main = do
    epoll <- create
    let sa = SockAddrUnix "/home/ellen/listensocket"
    s <- socket AF_UNIX Stream defaultProtocol
    bind s sa
    listen s 10
    let fd = fdSocket s
    epoll <- add epoll fd (fromOpts [EPOLLIN, EPOLLERR, EPOLLOUT]) (s, Listen)
    evts <- wait 4 epoll

    putStrLn "got an event"
