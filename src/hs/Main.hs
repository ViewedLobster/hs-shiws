import Control.Concurrent.MVar
import Control.Concurrent
import System.IO.Error
import qualified Data.ByteString as BS
import Foreign
import Foreign.Ptr
import Foreign.C.Error
import Foreign.C.String
import Data.Word
import Data.List

import Control.Monad
import Control.Applicative
import Control.Monad


import Lowl.EPoll (
      EPoll
    , EPollEvt (..)
    , EPollOpt (..)
    , create
    , register
    , unregister
    , wait )
import Lowl.Socket (
      Socket
    , SockDomain (..)
    , SockType (..)
    , SockProto (..)
    , SockAddress (..)
    , socketNoBlock
    , acceptNoBlock
    , bind
    , listen
    , unsafeRecvPtr
    , unsafeSendPtr
    , close
    , unsafeUsingSocketFd )

newtype RdySignal = RdySignal (MVar (Bool, [MVar ()]))

newRdySignal = RdySignal <$> (newMVar (False, []))

data WrappedSocket = AcceptSocket (EPoll WrappedSocket) Socket
                   | AsyncSocket (EPoll WrappedSocket) Socket RdySignal RdySignal

wrapSocket :: EPoll WrappedSocket -> Socket -> IO WrappedSocket
wrapSocket epoll sock = do
    r <- newRdySignal
    w <- newRdySignal
    let as = AsyncSocket epoll sock r w in
        unsafeUsingSocketFd sock $ \sfd -> do
            register epoll sfd [EPOLLIN, EPOLLOUT, EPOLLHUP, EPOLLRDHUP, EPOLLERR, EPOLLET, EPOLLPRI] as
            return as

wrapAccept :: EPoll WrappedSocket -> Socket -> IO WrappedSocket
wrapAccept epoll sock = do
    let as = AcceptSocket epoll sock in
        unsafeUsingSocketFd sock $ \sfd -> do
            register epoll sfd [EPOLLIN, EPOLLERR, EPOLLET] as
            return as

killSocket :: WrappedSocket -> IO ()
killSocket wrapped = do
    let (epoll, sock) = case wrapped of
                  AcceptSocket epoll sock       -> (epoll, sock)
                  AsyncSocket epoll sock _ _    -> (epoll, sock) in
        unsafeUsingSocketFd sock $ \sfd -> do
            unregister epoll sfd
            close sock

waitOrReset :: RdySignal -> IO ()
waitOrReset (RdySignal sig) = do
    (avail, waiting) <- takeMVar sig
    if avail then do
        putMVar sig (False, waiting)
        return ()
    else do
        empty <- newEmptyMVar
        putMVar sig (False, empty:waiting)
        takeMVar empty

wakeupOrSet :: RdySignal -> IO ()
wakeupOrSet (RdySignal sig) = do
    (_, waiting) <- takeMVar sig
    case waiting of
        first:rest -> do
            putMVar sig (False, rest)
            putMVar first ()
        _          ->
            putMVar sig (True, waiting)

asyncRecv :: WrappedSocket -> Int -> IO BS.ByteString
asyncRecv sock num = do
    allocaBytes num $ \ptr -> do
        res <- asyncRecvPtr sock ptr num
        BS.packCStringLen (castPtr ptr, res)

asyncRecvPtr :: WrappedSocket -> Ptr Word8 -> Int -> IO Int
asyncRecvPtr s@(AsyncSocket _ sock rdyRd _) ptr num = do
    waitOrReset rdyRd
    res <- unsafeRecvPtr sock ptr num
    if res == -1 then do
        errno <- getErrno
        if errno == eAGAIN || errno == eWOULDBLOCK then
            asyncRecvPtr s ptr num
        else
            throwErrno "unsafeRecvPtr failed"
    else return res

asyncSend :: WrappedSocket -> BS.ByteString -> IO ()
asyncSend s bs = BS.useAsCStringLen bs $ \(ptr, num) ->
    asyncSendPtr s (castPtr ptr) num

asyncSendPtr :: WrappedSocket -> Ptr Word8 -> Int -> IO ()
asyncSendPtr s@(AsyncSocket _ sock _ rdyWr) ptr num = do
    waitOrReset rdyWr
    res <- unsafeSendPtr sock ptr num
    if res == -1 then do
        errno <- getErrno
        if errno == eAGAIN || errno == eWOULDBLOCK then
            asyncSendPtr s ptr num
        else
            throwErrno "unsafeSendPtr failed"
    else if res /= num then
        asyncSendPtr s (ptr `plusPtr` res) (num - res)
    else return ()


-- Build some kind of monad containing request?

-- getType
-- getPath
-- getHeaders
-- getBody - this should return some kind of lazy datatype?
-- getBodyContent
--
newtype Request a = Request ((WrappedSocket, RequestInfo) -> IO a)

instance Functor Request where
    fmap = liftM

instance Applicative Request where
    pure a = Request (\ws -> return a)
    (<*>) = ap

instance Monad Request where
    Request now >>= after =
        Request $ \req ->
            now req >>= \val -> let Request next = after val in next req

execRequest :: Request a -> RequestInfo -> WrappedSocket -> IO a
execRequest (Request sockAct) req sock = sockAct (sock, req)

-- TODO parse start-line
-- TODO parse headers
-- TODO handle chunked encoding
-- TODO handle content length

parseStartLine :: ByteString -> (ByteString, HTTPMethod, ByteString)
parseStartLine = undefined

parseHeaders :: ByteString -> (ByteString, Headers)
parseHeaders = undefined


data HTTPMethod = Get
                | Post
                | Head
                | Delete

data RequestInfo = RequestInfo
    {
        method :: HTTPMethod
      , path :: BS.ByteString
      , headers :: [(BS.ByteString, BS.ByteString)]
    }

reqMethod :: Request HTTPMethod
reqMethod = Request $ \ (_, info) -> return (method info)

reqPath :: Request BS.ByteString
reqPath = Request $ \ (_, info) -> return (path info)

reqHeaders :: Request [(BS.ByteString, BS.ByteString)]
reqHeaders = Request $ \ (_, info) -> return (headers info)

reqBodyChunk :: Int -> Request BS.ByteString
reqBodyBytes = undefined

withReqBodyBytes :: ([Word8] -> a) -> Request a

withReqBodyChunks :: ([ByteString] -> a) -> Request a

handleRequest :: WrappedSocket -> IO ()
handleRequest sock = do
    bs <- asyncRecv sock 1024
    if BS.length bs > 0 then do
        asyncSend sock bs
        handleRequest sock
    else return ()

data EventSummary = SumRead
                  | SumWrite
                  | SumBoth
                  | SumBottom
    deriving (Eq, Show)

fromEPollEvent evt = case evt of
    EPollIn    -> SumRead
    EPollOut   -> SumWrite
    EPollErr   -> SumBoth
    EPollHup   -> SumBoth
    EPollRdHup -> SumBoth
    EPollPri   -> SumBoth

sumUnion _ SumBoth = SumBoth
sumUnion SumBoth _ = SumBoth

sumUnion SumRead SumWrite = SumBoth
sumUnion SumWrite SumRead = SumBoth

sumUnion SumBottom a = a
sumUnion a SumBottom = a

sumUnion a b | a == b = a

summary = (foldl' sumUnion SumBottom) . (map fromEPollEvent)

shutdownSocket wrapped (Left exc) = do
    putStrLn $ "error: " ++  show exc
    killSocket wrapped

shutdownSocket wrapped _ = killSocket wrapped

handleEPollEvent epoll (evts, sock) = case sock of
    AsyncSocket epoll sock rd wr ->
        case summary evts of
            SumBoth -> do wakeupOrSet rd
                          wakeupOrSet wr
            SumRead -> wakeupOrSet rd
            _       -> wakeupOrSet wr
    AcceptSocket epoll asock ->
        case summary evts of
            SumRead -> do
                (sock, sockaddr) <- acceptNoBlock asock
                wrapped <- wrapSocket epoll sock
                forkFinally (handleRequest wrapped) (shutdownSocket wrapped)
                putStrLn $ "forked thread handling request from: " ++ show sockaddr
            SumBoth -> ioError (userError "accepting socket encountered an error")


infinitely action = action >> infinitely action

main = do
    putStrLn "Starting server"
    epoll <- create
    asock <- socketNoBlock AF_INET SOCK_STREAM Default
    bind asock $ InetAddress 0x7f000001 1234
    listen asock 64
    wrapped <- wrapAccept epoll asock
    let handleEpoll = wait 20 epoll >>= mapM (handleEPollEvent epoll) in
        catchIOError (infinitely handleEpoll)
                     (\exc -> putStrLn ("Main error: " ++ show exc) >> killSocket wrapped)

