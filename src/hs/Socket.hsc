{-# LANGUAGE CPP, CApiFFI, ForeignFunctionInterface #-}

module Lowl.Socket
where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.Error
import Data.IORef
-- import Data.ByteString

#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <unistd.h>
#include <fcntl.h>

data SockDomain = AF_UNIX
                | AF_INET
                | AF_INET6
    deriving (Eq, Show)

data SockType = SOCK_STREAM
              | SOCK_DGRAM
    deriving (Eq, Show)

data SockProto = Default
    deriving (Eq, Show)

fromDomain :: SockDomain -> CInt
fromDomain d = case d of
    AF_UNIX  -> (#const AF_UNIX) :: CInt
    AF_INET  -> (#const AF_INET) :: CInt
    AF_INET6 -> (#const AF_INET6) :: CInt

fromType :: SockType -> CInt
fromType t = case t of
    SOCK_STREAM -> (#const SOCK_STREAM) :: CInt
    SOCK_DGRAM  -> (#const SOCK_DGRAM)  :: CInt

fromProto p = case p of
    Default -> 0 :: CInt

data SockState = SockOpen
               | SockClosed
    deriving (Eq, Show)

data SockInfo = SockInfo CInt SockState SockDomain
    deriving (Eq, Show)

data SockAddress = InetAddress Word32 Word16
    deriving (Eq, Show)

peekInet ptr = do
    a <- c_ntohl <$> (#peek struct sockaddr_in, sin_addr.s_addr) ptr
    p <- c_ntohs <$> (#peek struct sockaddr_in, sin_port) ptr
    return (InetAddress a p)

instance Storable SockAddress where
    -- InetAddress
    poke ptr (InetAddress addr port) = do
        -- sa_family_t is unsigned short int
        (#poke struct sockaddr_in, sin_family) ptr ((#const AF_INET) :: CUShort)
        (#poke struct sockaddr_in, sin_port) ptr (c_htons port)
        (#poke struct sockaddr_in, sin_addr.s_addr) ptr (c_htonl addr)
    peek = peekInet
    alignment (InetAddress _ _) = #alignment struct sockaddr_in
    sizeOf (InetAddress _ _) = #size struct sockaddr_in


newtype Socket = Socket (IORef SockInfo)

foreign import ccall unsafe "arpa/inet.h htonl"
    c_htonl :: Word32 -> Word32
foreign import ccall unsafe "arpa/inet.h htons"
    c_htons :: Word16 -> Word16
foreign import ccall unsafe "arpa/inet.h ntohl"
    c_ntohl :: Word32 -> Word32
foreign import ccall unsafe "arpa/inet.h ntohs"
    c_ntohs :: Word16 -> Word16

-- Should this really be unsafe?
foreign import ccall unsafe "sys/socket.h socket"
    c_socket :: CInt -> CInt -> CInt -> IO CInt
    -- c_socket domain type proto
foreign import ccall unsafe "sys/socket.h bind"
    c_bind :: CInt -> Ptr SockAddress -> Word32 -> IO CInt
    -- c_bind sockfd addrptr ptrlen
foreign import ccall unsafe "sys/socket.h listen"
    c_listen :: CInt -> CInt -> IO CInt
    -- c_listen sockfd num
foreign import ccall unsafe "sys/socket.h accept"
    c_accept :: CInt -> Ptr SockAddress -> Ptr Word32 -> IO CInt
foreign import ccall unsafe "sys/socket.h accept4"
    c_accept4 :: CInt -> Ptr SockAddress -> Ptr (#type socklen_t) -> CInt -> IO CInt
    -- c_accept sockfd addrptr ptrlen
--foreign import ccall unsafe "sys/socket.h setsockopt"
--    c_setsockopt :: CInt -> CInt -> CInt -> Ptr SockOpt -> Word32 -> IO ()
--    -- c_setsockopt sockfd level optname optval optlen
foreign import ccall unsafe "unistd.h close"
    c_close :: CInt -> IO CInt
foreign import capi unsafe "fcntl.h fcntl"
    c_fcntl3 :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "sys/socket.h recv"
    c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO #type ssize_t
foreign import ccall unsafe "sys/socket.h send"
    c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO #type ssize_t

-- TODO is O_CLOEXEC something we want to set as well?
-- TODO check socket state and fail early
socketNoBlock :: SockDomain -> SockType -> SockProto -> IO Socket
socketNoBlock dom tpe proto = do
    ret <- c_socket (fromDomain dom) (fromType tpe) (fromProto proto)
    if ret == -1 then
        throwErrno "socket() failed"
    else do
        setNoBlockFd ret
        putStrLn $ "socketNoBlock created fd: " ++ show ret
        info <- newIORef (SockInfo ret SockOpen dom)
        return (Socket info)

bind :: Socket -> SockAddress -> IO ()
bind (Socket ref) sa = do
    SockInfo fd _ _ <- readIORef ref
    res <- allocaBytes (sizeOf sa) $ \ptr -> do
        poke ptr sa
        c_bind fd ptr (fromIntegral (sizeOf sa))
    if res == -1 then
        throwErrno "bind() failed"
    else
        return ()

listen :: Socket -> Int -> IO ()
listen (Socket ref) num = do
    SockInfo fd _ _ <- readIORef ref
    res <- c_listen fd (fromIntegral num)
    if res == -1 then
        throwErrno "listen() failed"
    else
        return ()

addrAlign dom = case dom of
    AF_INET -> alignment (InetAddress 0 0)

addrSize dom = case dom of
    AF_INET -> sizeOf (InetAddress 0 0)

peekAddr dom _ = case dom of
    AF_INET -> peekInet

-- Blocks if socket is non-blocking, use with caution
-- check for EAGAIN and EWOULDBLOCK
acceptNoBlock :: Socket -> IO (Socket, SockAddress)
acceptNoBlock (Socket ref) = do
    SockInfo fd _ dom <- readIORef ref
    let sz = addrSize dom in
        allocaBytes sz $ \ptr -> alloca $ \ptrlen -> do
            poke ptrlen (fromIntegral sz)
            res <- c_accept4 fd ptr ptrlen (#const SOCK_NONBLOCK)
            if res == -1 then do
                throwErrno "accept4()"
            else do
                addrlen <- peek ptrlen
                addr <- peekAddr dom addrlen ptr
                ref <- newIORef (SockInfo res SockOpen dom)
                return (Socket ref, addr)

f_GETFD = (#const F_GETFD) :: CInt
f_SETFD = (#const F_SETFD) :: CInt

setNoBlock :: Socket -> IO ()
setNoBlock (Socket ref) = do
    SockInfo fd _ _ <- readIORef ref
    setNoBlockFd fd

setNoBlockFd :: CInt -> IO ()
setNoBlockFd fd = do
    res <- c_fcntl3 fd f_GETFD 0
    if res == -1 then
        throwErrno "fcntl(F_GETFD)"
    else do
        res <- c_fcntl3 fd f_SETFD (res .|. (#const O_NONBLOCK))
        if res == -1 then
            throwErrno "fcntl(F_SETFD)"
        else return ()

replayIfInterruptedThrow syscall errmesg = do
    ret <- replayIfInterrupted syscall
    if ret == -1 then
        throwErrno errmesg
    else return ret

replayIfInterrupted syscall = do
    ret <- syscall
    if ret == -1 then do
        errno <- getErrno
        if errno == eINTR then
            replayIfInterrupted syscall
        else
            return ret
    else return ret

close :: Socket -> IO ()
close (Socket ref) = do
    SockInfo fd _ dom <- readIORef ref
    res <- replayIfInterruptedThrow (c_close fd) "close()"
    writeIORef ref (SockInfo fd SockClosed dom)

closeOnce (Socket ref) = do
    SockInfo fd st dom <- readIORef ref
    case st of
        SockOpen   -> do res <- replayIfInterruptedThrow (c_close fd) "close()"
                         writeIORef ref (SockInfo fd SockClosed dom)
        SockClosed -> return ()

socketFd :: Socket -> IO CInt
socketFd (Socket ref) = do
    SockInfo fd _ _ <- readIORef ref
    return fd

unsafeUsingSocketFd :: Socket -> (CInt -> IO a) -> IO a
unsafeUsingSocketFd (Socket ref) f = do
    SockInfo fd _ _ <- readIORef ref
    f fd

--unsafeRecv :: Socket -> Int -> IO (Either Errno ByteString)
--unsafeRecv (Socket ref) num = do
--    -- TODO if num < 0...
--    SockInfo fd _ _ <- readIORef ref
--    allocaBytes num $ \ptr -> do
--        res <- replayIfInterrupted (c_recv fd ptr (fromIntegral num) 0)
--        if res == -1 then
--            Left <$> getErrno
--        else
--            Right <$> packCStringLen (castPtr ptr, fromIntegral res)

unsafeRecvPtr :: Socket -> Ptr Word8 -> Int -> IO Int
unsafeRecvPtr (Socket ref) ptr num = do
    SockInfo fd _ _ <- readIORef ref
    fromIntegral <$> replayIfInterrupted (c_recv fd ptr (fromIntegral num) 0)

unsafeSendPtr :: Socket -> Ptr Word8 -> Int -> IO Int
unsafeSendPtr (Socket ref) ptr num = do
    SockInfo fd _ _ <- readIORef ref
    fromIntegral <$> replayIfInterrupted (c_send fd ptr (fromIntegral num) 0)


