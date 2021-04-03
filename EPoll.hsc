{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module EPoll (
      EPoll
    , EPollEvt
    , EPollOpts
    , create
    , add
    , del
    , mod
) where

import Data.Word (Word32)
import Data.Foldable
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.Error
import Data.Bits
import Control.Concurrent.MVar
import Control.Concurrent.STM

#include <sys/epoll.h>

-- Events

data EPollEvt = EPollIn
              | EPollOut
              | EPollRdHup
              | EPollPri
              | EPollErr
              | EPollHup

allEvts = [EPollIn, EPollOut, EPollRdHup, EPollPri, EPollErr, EPollHup]
fromEvt evt = case evt of
    EPollIn     -> (#const EPOLLIN) :: Word32
    EPollOut    -> (#const EPOLLOUT) :: Word32
    EPollRdHup  -> (#const EPOLLRDHUP) :: Word32
    EPollPri    -> (#const EPOLLPRI) :: Word32
    EPollErr    -> (#const EPOLLERR) :: Word32
    EPollHup    -> (#const EPOLLHUP) :: Word32

toEvts :: Word32 -> [EPollEvt]
toEvts evtWord = filter (\evt -> (evtWord .&. fromEvt evt) /= 0) allEvts

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

-- TODO is there a better solution than TVar map
-- TODO investigate use of Fd type?
data EPoll a = EPoll CInt (TVar (Map CInt a))

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
        fdmap <- atomically (do fdmap <- newTVar Map.empty; return fdmap)
        return (EPoll fd fdmap)

-- Close
close :: EPoll a -> IO ()
foreign import ccall unsafe "unistd.h close" c_close :: CInt -> IO ()

close (EPoll fd _) = doc_close fd

-- Wait
wait :: Int -> EPoll a -> IO [([EPollEvt], a)]

foreign import ccall safe "sys/epoll.h epoll_wait"
    c_epoll_wait :: CInt -> Ptr EPollEvent -> CInt -> CInt -> IO CInt

waitInternal :: CInt -> Int -> (Ptr EPollEvent) -> IO [(Word32, CInt)]
waitInternal epollfd num ptr = do
    res <- c_epoll_wait epollfd ptr (fromIntegral num) (fromIntegral (-1))
    let getEvts ptr rem = do
            if rem == 0 then
                return []
            else do
                rest <- getEvts (ptr `plusPtr` evtsize) (rem - 1)
                EPollEvent evts fd <- peek ptr
                return ((evts, fd) : rest)
        in if res == -1 then do
               errno <- getErrno
               if errno == eINTR then do
                   putStrLn "EPoll interrupted"
                   waitInternal epollfd num ptr
               else
                   throwErrno "Call to epoll_wait failed" -- TODO improve error handling
           else
               getEvts ptr res


wait num (EPoll fd fdmap') = do
    evts <- allocaBytes (num * evtsize) (waitInternal fd num)
    mapped <- atomically (do
        fdmap <- readTVar fdmap'
        -- TODO use safe Map get
        let mapEvt (evtWord, fdint) = (toEvts evtWord, fdmap ! fdint)
            in return (map mapEvt evts))
    return mapped


-- TODO add error handling, look at Foreign.C.Error

-- Ctl

foreign import ccall unsafe "sys/epoll.h epoll_ctl"
    c_epoll_ctl :: CInt -> CInt -> CInt -> Ptr EPollEvent -> IO CInt

-- TODO revert stm ops on error for add, del and mod

add :: EPoll a -> CInt -> Word32 -> a -> IO ()
add (EPoll epollfd fdmap') fd opts d = do
    member <- atomically (do
        fdmap <- readTVar fdmap'
        if not (Map.member fd fdmap) then do
            writeTVar fdmap' (Map.insert fd d fdmap)
            return True
        else
            return False)

    if member then do
        res <- alloca $ \ptr -> do
            poke ptr (EPollEvent opts fd)
            c_epoll_ctl epollfd (#const EPOLL_CTL_ADD) fd ptr
        if res == -1 then do
            throwErrno "epoll_ctl(EPOLL_CTL_ADD) call failed"
        else
            return ()
    else
        error "add: fd member of fdmap"

del :: EPoll a -> CInt -> IO ()
del (EPoll epollfd fdmap') fd = do
    member <- atomically (do
        fdmap <- readTVar fdmap'
        if Map.member fd fdmap then do
            writeTVar fdmap' (Map.delete fd fdmap)
            return True
        else
            return False)
    
    if member then do
        res <- c_epoll_ctl epollfd (#const EPOLL_CTL_DEL) fd nullPtr
        if res == -1 then do
            throwErrno "epoll_ctl(EPOLL_CTL_DEL) call failed"
        else
            return ()
    else
        error "del: fd not member of fdmap"

mod :: EPoll a -> CInt -> Word32 -> a ->  IO ()
mod (EPoll epollfd fdmap') fd opts d = do
    member <- atomically (do
        fdmap <- readTVar fdmap'
        if Map.member fd fdmap then do
            writeTVar fdmap' (Map.insert fd d fdmap)
            return True
        else
            return False)

    if member then do
        res <- alloca $ \ptr -> do
            poke ptr (EPollEvent opts fd)
            c_epoll_ctl epollfd (#const EPOLL_CTL_MOD) fd ptr
        if res == -1 then do
            throwErrno "epoll_ctl(EPOLL_CTL_MOD) call failed"
        else
            return ()
    else
        error "mod: fd not member of fdmap"

