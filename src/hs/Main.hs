{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.MVar
import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder as B
import Foreign
import Foreign.Ptr
import Foreign.C.Error
import Foreign.C.String
import Data.Word
import Data.List

import Control.Monad
import Control.Applicative

import Control.Monad.State

import System.IO.Error
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Console.GetOpt
import System.Environment

import qualified Lowl.EPoll as EP
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

import HTTPParse (
      runParser
    , HTTPMethod
    , HTTPStart (..)
    , httpReqStart
    , httpHeaders
    , httpHeaderFields
    , httpRequestInfo )

newtype RdySignal = RdySignal (MVar (Bool, [MVar ()]))

newRdySignal = RdySignal <$> (newMVar (False,  []))

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

asyncRecv :: WrappedSocket -> Int -> IO ByteString
asyncRecv sock num = do
    allocaBytes num $ \ptr -> do
        res <- asyncRecvPtr sock ptr num
        BS.packCStringLen (castPtr ptr, res)

asyncRecvPtr :: WrappedSocket -> Ptr Word8 -> Int -> IO Int
asyncRecvPtr s@(AsyncSocket _ sock rdyRd _) ptr num = do
    res <- unsafeRecvPtr sock ptr num
    if res == -1 then do
        errno <- getErrno
        if errno == eAGAIN || errno == eWOULDBLOCK then do
            waitOrReset rdyRd
            asyncRecvPtr s ptr num
        else
            throwErrno "unsafeRecvPtr failed"
    else return res

asyncSend :: WrappedSocket -> ByteString -> IO ()
asyncSend s bs = do
    putStrLn $ "sending: " ++ show bs
    BS.useAsCStringLen bs $ \(ptr, num) ->
        asyncSendPtr s (castPtr ptr) num

asyncSendPtr :: WrappedSocket -> Ptr Word8 -> Int -> IO ()
asyncSendPtr s@(AsyncSocket _ sock _ rdyWr) ptr num = do
    res <- unsafeSendPtr sock ptr num
    if res == -1 then do
        errno <- getErrno
        if errno == eAGAIN || errno == eWOULDBLOCK then do
            waitOrReset rdyWr
            asyncSendPtr s ptr num
        else
            throwErrno "unsafeSendPtr failed"
    else if res /= num then
        asyncSendPtr s (ptr `plusPtr` res) (num - res)
    else return ()

-- TODO handle chunked encoding
-- TODO handle content length
-- TODO handle lazy bytestring stuff

lazySocketBytes :: WrappedSocket -> Int -> IO L.ByteString
lazySocketBytes sock chunkSize = unsafeInterleaveIO $ do
    putStrLn $ "receiving chunk"
    chunk <- asyncRecv sock chunkSize
    putStrLn $ "received chunk" ++ show chunk
    if BS.length chunk == 0 then
        return L.empty
    else do
        rest <- lazySocketBytes sock chunkSize
        putStrLn $ "got rest"
        return $ L.fromChunks ( chunk:(L.toChunks rest))

lazySocketChunks :: WrappedSocket -> Int -> IO [ByteString]
lazySocketChunks sock chunkSize = unsafeInterleaveIO $ do
    putStrLn $ "receiving chunk"
    chunk <- asyncRecv sock chunkSize
    putStrLn $ "received chunk" ++ show chunk
    if BS.length chunk == 0 then
        return []
    else do
        rest <- lazySocketChunks sock chunkSize
        putStrLn $ "got rest"
        return $ chunk:rest

headerMax = 8192

--handleRequests :: WrappedSocket -> IO ()
--handleRequests sock = do
--    chunks <- lazySocketChunks sock 4096
--    putStrLn "lazysocketbytes"
--    let bytes = L.fromChunks chunks
--    case runParser httpRequestInfo bytes of
--        ((start, headers), afterHead):_ -> do
--            putStrLn $ show headers
--            helloWorld sock
--        _                               -> httpError400 sock
--

-- TODO timeout for receive

-- read bytes, parse, read bytes parse...
--

data Request = Request
    {
        method :: HTTPMethod
      , path :: ByteString
      , headers :: [(ByteString, ByteString)]
      , body :: L.ByteString
    }

data ResponseCode = OK
                  | BadRequest
                  | InternalError

toBytes :: ResponseCode -> B.Builder
toBytes OK              = "200 OK"
toBytes BadRequest      = "400 Bad Request"
toBytes InternalError   = "500 Internal Error"

data Response = ResponseBytes ResponseCode [(ByteString, ByteString)] B.Builder
              | ResponseError ResponseCode

headersToBytes =
    foldl (<>) "" .
    map (\ (name, val) ->
           (byteString name) <> ": " <> (byteString val) <> "\r\n")

respond :: WrappedSocket -> Response -> IO ()
respond sock (ResponseBytes code hdrs bytes) = do
    let bs = B.toLazyByteString $ "HTTP/1.1 " <> toBytes code <> "\r\n" <> headersToBytes hdrs <> "\r\n" <> bytes
    mapM_ (asyncSend sock) (L.toChunks bs)

respond sock (ResponseError code) = do
    let bs = B.toLazyByteString $ "HTTP/1.1 " <> toBytes code <> "\r\n\r\n"
    mapM_ (asyncSend sock) (L.toChunks bs)

constructStart sock bytes = do
    let res = runParser httpReqStart bytes
    case res of
        (start, rest):_ -> return (Just start, rest)
        _   -> do
            new <- asyncRecv sock 8192
            constructStart sock (bytes <> new)

constructHeaders sock bytes = tryHeaders sock bytes 0

maxTriesHdrs = 5
tryHeaders sock bytes try =
    if try >= maxTriesHdrs then
        return (Nothing, bytes)
    else
        case runParser httpHeaders bytes of
            ((hdrs, True), rest):_ -> return (Just hdrs, rest)
            ((hdrs, False), rest):_ -> do
                new <- asyncRecv sock 8192
                more <- tryHeaders sock (rest <> new) (try + 1)
                case more of
                    (Just mhdrs, rest) -> return (Just (hdrs ++ mhdrs), rest)
                    _                  -> return more

constructRequest sock bytes = do
    (res, rest) <- constructStart sock bytes -- TODO make constructStart count tries
    case res of
        Just (HTTPReqStart meth path version) -> do
            (res, rest) <- constructHeaders sock rest
            case res of
                Just hdrs -> do
                    (bdy, rest) <- lazyBody hdrs rest sock
                    return (Just (Request meth path hdrs bdy), rest)
                _ -> return (Nothing, rest)
        _ -> return (Nothing, rest)

lazyBody :: [(ByteString, ByteString)] -> ByteString -> WrappedSocket -> IO (L.ByteString, ByteString)
lazyBody _ _ _ = return (L.empty, BS.empty) -- TODO fix this

handleRequest :: WrappedSocket -> (Request -> (Response -> IO ()) -> IO ()) -> IO ()
handleRequest sock userRequest = do
    (res, rest) <- constructRequest sock BS.empty
    case res of
        Just req -> userRequest req (respond sock)
        Nothing -> respond sock (ResponseError BadRequest)

helloWorld sock = asyncSend sock "HTTP/1.1 200 OK\r\nContent-Length: 15\r\n\r\nHello, world!\r\n"

httpError400 sock =
    asyncSend sock "HTTP/1.1 400 Bad Request"

-- lazy socket layer
-- updating state to point to non-consumed data
-- RequestData: takeN

-- Build some kind of monad containing request?

-- getType
-- getPath
-- getHeaders
-- getBodyContent
--
--newtype Request a = Request ((WrappedSocket, RequestInfo) -> IO a)
--
--instance Functor Request where
--    fmap = liftM
--
--instance Applicative Request where
--    pure a = Request (\ws -> return a)
--    (<*>) = ap
--
--instance Monad Request where
--    Request now >>= after =
--        Request $ \req ->
--            now req >>= \val -> let Request next = after val in next req
--
--execRequest :: Request a -> RequestInfo -> WrappedSocket -> IO a
--execRequest (Request sockAct) req sock = sockAct (sock, req)
--
--data RequestInfo = RequestInfo
--    {
--        method :: HTTPMethod
--      , path :: ByteString
--      , headers :: [(ByteString, ByteString)]
--      , body :: L.ByteString
--    }
--
--reqMethod :: Request HTTPMethod
--reqMethod = Request $ \ (_, info) -> return (method info)
--
--reqPath :: Request ByteString
--reqPath = Request $ \ (_, info) -> return (path info)
--
--reqHeaders :: Request [(ByteString, ByteString)]
--reqHeaders = Request $ \ (_, info) -> return (headers info)

--handleRequest :: WrappedSocket -> IO ()
--handleRequest sock = do
--    bs <- asyncRecv sock 1024
--    if BS.length bs > 0 then do
--        asyncSend sock bs
--        handleRequest sock
--    else return ()
--

userRequestTest req resp = do
    resp (ResponseBytes OK [("Content-Length", "15")] "Hello, world!\r\n")

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

acceptConnections :: EPoll WrappedSocket -> Socket -> IO ()
acceptConnections epoll asock = do
    accepted <- acceptNoBlock asock
    case accepted of
        Just (sock, sockaddr) -> do
            wrapped <- wrapSocket epoll sock
            forkFinally (handleRequest wrapped userRequestTest) (shutdownSocket wrapped)
            putStrLn $ "forked thread handling request from: " ++ show sockaddr
            acceptConnections epoll asock
        _ -> return ()

handleEPollEvent epoll (evts, sock) = case sock of
    AsyncSocket epoll sock rd wr ->
        case summary evts of
            SumBoth -> do wakeupOrSet rd
                          wakeupOrSet wr
                          putStrLn $ "read/write event on " ++ show epoll
            SumRead -> do wakeupOrSet rd
                          putStrLn $ "read event on " ++ show epoll
            _       -> do wakeupOrSet wr
                          putStrLn $ "write event on " ++ show epoll
    AcceptSocket epoll asock ->
        case summary evts of
            SumRead -> acceptConnections epoll asock
            SumBoth -> ioError (userError "accepting socket encountered an error")


infinitely action = action >> infinitely action

data ServerOptions = ServerOptions {
    address :: Word32,
    port :: Word16
}

options =
 [
--    Option ['a'] ["address"] (ReqArg (\val opts -> parseAddress val >>= \addr ->
--                                                   return (opts { address = addr })) "ADDRESS") "bind address ADDRESS",
    Option ['p']
           ["port"]
           (ReqArg (\val opts -> (let parsed = (read val) :: Int in
                                    if parsed > 0xffff || parsed <= 0 then
                                        Left "invalid port number"
                                    else
                                        Right (fromIntegral parsed)) >>= \p ->
                                            return (opts { port = p })) "PORTNUM")
           "bind port PORTNUM"
 ]

defaultOptions = ServerOptions { address = 0, port = 80 }

config argv =
    case getOpt Permute options argv of
        (o,n,[]) -> case foldl (>>=) (Right defaultOptions) o of
                        Right opt -> return opt
                        Left err -> ioError (userError (err ++ usage))
        (_,_,errs) -> ioError (userError (concat errs ++ usage))
  where usage = " (Valid options: -p PORTNUM, -a ADDRESS)"

main = do
    argv <- getArgs
    opts <- config argv
    putStrLn $ "Starting server on port " ++ show (port opts)
    epoll <- create
    asock <- socketNoBlock AF_INET SOCK_STREAM Default
    bind asock $ InetAddress (address opts) (port opts)
    listen asock 64
    wrapped <- wrapAccept epoll asock
    let handleEpoll = wait 20 epoll >>= mapM (handleEPollEvent epoll) in
        catchIOError (infinitely handleEpoll)
                     (\exc -> putStrLn ("Main error: " ++ show exc) >>
                              killSocket wrapped >> EP.close epoll)

