{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module EPoll where

import Data.Word (Word32)
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.Error
import Data.Bits
import Control.Concurrent.MVar

#include <sys/epoll.h>

-- Events

data EPollEventT = EPollIn
                 | EPollOut
                 | EPollRdHup
                 | EPollPri
                 | EPollErr
                 | EPollHup

-- Constants
data EPollOpt = EPOLLIN
              | EPOLLPRI
              | EPOLLOUT
              | EPOLLERR
              | EPOLLHUP
              | EPOLLRDHUP
              | EPOLLET

allOpts = [EPOLLIN, EPOLLPRI, EPOLLOUT, EPOLLERR, EPOLLHUP, EPOLLRDHUP, EPOLLET]

fromOpt o = case o of
    EPOLLIN     -> (#const EPOLLIN) :: Word32
    EPOLLPRI    -> (#const EPOLLPRI) :: Word32
    EPOLLOUT    -> (#const EPOLLOUT) :: Word32
    EPOLLERR    -> (#const EPOLLERR) :: Word32
    EPOLLHUP    -> (#const EPOLLHUP) :: Word32
    EPOLLRDHUP  -> (#const EPOLLRDHUP) :: Word32
    EPOLLET     -> (#const EPOLLET) :: Word32

fromOpts :: [EPollOpt] -> Word32
fromOpts = foldl' (\x y -> x .|. (fromOpt y)) (0 :: Word32)

toOpts :: Word32 -> [EPollOpt]
toOpts oWord = filter (\o -> (fromOpt o .&. oWord) /= 0) allOpts 

-- TODO this sucks, cannot have an MVar here... At least we avoid race conditions but that's about it
-- TODO investigate use of Fd type?
data EPoll a = EPoll CInt (MVar (Map CInt a))

-- TODO handle multiple calls to ctl, we have no reference to data assigned to file descriptor

data EPollEvent = EPollEvent
    { evts :: Word32
    , d :: CInt
    }

instance Storable EPollEvent where
    peek ptr = EPollEvent
        <$> (#peek struct epoll_event, events) ptr
        <*> (#peek struct epoll_event, data) ptr
    poke ptr (EPollEvent evts d) = do
        (#poke struct epoll_event, events) ptr evts
        (#poke struct epoll_event, data) ptr d
    alignment _ = #alignment struct epoll_event
    sizeOf _    = #size struct epoll_event

evtsize = sizeOf (undefined :: EPollEvent)
evtalign = alignment (undefined :: EPollEvent)

-- Create

create :: IO (EPoll a)
-- TODO check if this can be marked as unsafe, i.e. if it fulfills 8.4.3 Import Declarations
foreign import ccall unsafe "sys/epoll.h epoll_create"
    c_epoll_create :: CInt -> IO CInt

create = do
    fd <- c_epoll_create 1
    if fd == -1 then do
        throwErrno "call to epoll_create failed"
    else do
        mv <- newMVar Map.empty
        return (EPoll fd mv)

-- Close
close :: EPoll a -> IO ()
foreign import ccall unsafe "unistd.h close" c_close :: CInt -> IO ()

close (EPoll fd _) = c_close fd

-- Wait
wait :: Int -> EPoll a -> IO [(Word32, a)]

foreign import ccall safe "sys/epoll.h epoll_wait"
    c_epoll_wait :: CInt -> Ptr EPollEvent -> CInt -> CInt -> IO CInt

waitInternal :: (Map CInt a) -> CInt -> Int -> (Ptr EPollEvent) -> IO [(Word32, a)]
waitInternal fdmap epollfd num ptr = do
    res <- c_epoll_wait epollfd ptr (fromIntegral num) (fromIntegral (-1))
    let getEvts ptr rem = do
            if rem == 0 then
                return []
            else do
                rest <- getEvts (ptr `plusPtr` evtsize) (rem - 1)
                EPollEvent evts fd <- peek ptr
                return ((evts, fdmap Map.! fd) : rest)
        in if res == -1 then do
               errno <- getErrno
               if errno == eINTR then do
                   putStrLn "Interrupted"
                   waitInternal fdmap epollfd num ptr
               else
                   throwErrno "Call to epoll_wait failed" -- TODO improve error handling
           else
               getEvts ptr res


wait num (EPoll fd fdmap') = do
    fdmap <- takeMVar fdmap'
    evts <- allocaBytes (num * evtsize) (waitInternal fdmap fd num)
    putMVar fdmap' fdmap
    return evts


-- TODO add error handling, look at Foreign.C.Error

-- Ctl

add :: EPoll a -> CInt -> Word32 -> a -> IO ()

foreign import ccall unsafe "sys/epoll.h epoll_ctl"
    c_epoll_ctl :: CInt -> CInt -> CInt -> Ptr EPollEvent -> IO CInt

add (EPoll epollfd fdmap') fd opts d = do
    fdmap <- takeMVar fdmap'
    if Map.notMember fd fdmap then do
        res <- alloca $ \ptr -> do
            poke ptr (EPollEvent opts fd)
            c_epoll_ctl epollfd (#const EPOLL_CTL_ADD) fd ptr
        putMVar fdmap' (Map.insert fd d fdmap)
        if res == -1 then do
            throwErrno "epoll_ctl(EPOLL_CTL_ADD) call failed"
        else
            return ()
    else
        error "add: fd already member of fdmap"

del :: EPoll a -> CInt -> IO ()
del (EPoll epollfd fdmap') fd = do
    fdmap <- takeMVar fdmap'
    if Map.member fd fdmap then do
        res <- c_epoll_ctl epollfd (#const EPOLL_CTL_DEL) fd nullPtr
        putMVar fdmap' (Map.delete fd fdmap)
        if res == -1 then do
            throwErrno "epoll_ctl(EPOLL_CTL_DEL) call failed"
        else
            return ()
    else
        error "del: fd not member of fdmap"

mod :: EPoll a -> CInt -> Word32 -> a ->  IO ()
mod (EPoll epollfd fdmap') fd opts d = do
    fdmap <- takeMVar fdmap'
    if Map.member fd fdmap then do
        res <- alloca $ \ptr -> do
            poke ptr (EPollEvent opts fd)
            c_epoll_ctl epollfd (#const EPOLL_CTL_MOD) fd ptr
        putMVar fdmap' (Map.insert fd d fdmap)
        if res == -1 then do
            throwErrno "epoll_ctl(EPOLL_CTL_MOD) call failed"
        else
            return ()
    else
        error "mod: fd not member of fdmap"

