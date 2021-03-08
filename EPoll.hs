{-# LINE 1 "EPoll.hsc" #-}
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
    EPOLLIN     -> (1) :: Word32
{-# LINE 39 "EPoll.hsc" #-}
    EPOLLPRI    -> (2) :: Word32
{-# LINE 40 "EPoll.hsc" #-}
    EPOLLOUT    -> (4) :: Word32
{-# LINE 41 "EPoll.hsc" #-}
    EPOLLERR    -> (8) :: Word32
{-# LINE 42 "EPoll.hsc" #-}
    EPOLLHUP    -> (16) :: Word32
{-# LINE 43 "EPoll.hsc" #-}
    EPOLLRDHUP  -> (8192) :: Word32
{-# LINE 44 "EPoll.hsc" #-}
    EPOLLET     -> (2147483648) :: Word32
{-# LINE 45 "EPoll.hsc" #-}

fromOpts :: [EPollOpt] -> Word32
fromOpts = foldl' (\x y -> x .|. (fromOpt y)) (0 :: Word32)

toOpts :: Word32 -> [EPollOpt]
toOpts oWord = filter (\o -> (fromOpt o .&. oWord) /= 0) allOpts 

data EPoll a = EPoll CInt (Map CInt a)

-- TODO handle multiple calls to ctl, we have no reference to data assigned to file descriptor

data EPollEvent = EPollEvent
    { evts :: Word32
    , d :: CInt
    }

instance Storable EPollEvent where
    peek ptr = EPollEvent
        <$> ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 64 "EPoll.hsc" #-}
        <*> ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 65 "EPoll.hsc" #-}
    poke ptr (EPollEvent evts d) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr evts
{-# LINE 67 "EPoll.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr d
{-# LINE 68 "EPoll.hsc" #-}
    alignment _ = 1
{-# LINE 69 "EPoll.hsc" #-}
    sizeOf _    = (12)
{-# LINE 70 "EPoll.hsc" #-}

evtsize = sizeOf (undefined :: EPollEvent)
evtalign = alignment (undefined :: EPollEvent)

-- Create

create :: IO (EPoll a)
-- TODO check if this can be marked as unsafe, i.e. if it fulfills 8.4.3 Import Declarations
foreign import ccall unsafe "sys/epoll.h epoll_create"
    c_epoll_create :: CInt -> IO CInt

create = do
    fd <- c_epoll_create 1
    if fd == -1 then
        throwErrno "call to epoll_create failed"
    else
        return (EPoll fd Map.empty)

-- Close
close :: EPoll a -> IO ()
foreign import ccall unsafe "unistd.h close" c_close :: CInt -> IO ()

close (EPoll fd _) = c_close fd

-- Wait
wait :: Int -> EPoll a -> IO [(Word32, a)]

foreign import ccall unsafe "sys/epoll.h epoll_wait"
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


wait num (EPoll fd fdmap) = do
    evts <- allocaBytes (num * evtsize) (waitInternal fdmap fd num)
    return evts


-- TODO can we store a pointer directly in event, this should quicken access later.
-- Requirement: if we use stable pointers we need to make sure they are released
-- Could probably make this part of cleanup of thread, we should just make sure data is never accessed later
-- maybe signal epoll thread to remove stuff


-- TODO add error handling, look at Foreign.C.Error

-- Ctl

add :: EPoll a -> CInt -> Word32 -> a -> IO (EPoll a)

foreign import ccall unsafe "sys/epoll.h epoll_ctl"
    c_epoll_ctl :: CInt -> CInt -> CInt -> Ptr EPollEvent -> IO CInt

add (EPoll epollfd fdmap) fd opts d = do
    if Map.notMember fd fdmap then do
        res <- alloca $ \ptr -> do
            poke ptr (EPollEvent opts fd)
            c_epoll_ctl epollfd (1) fd ptr
{-# LINE 146 "EPoll.hsc" #-}
        if res == -1 then
            throwErrno "epoll_ctl(EPOLL_CTL_ADD) call failed"
        else
            return (EPoll epollfd (Map.insert fd d fdmap))
    else
        error "add: fd already member of fdmap"

del :: EPoll a -> CInt -> IO (EPoll a)
del (EPoll epollfd fdmap) fd = do
    if Map.member fd fdmap then do
        res <- c_epoll_ctl epollfd (2) fd nullPtr
{-# LINE 157 "EPoll.hsc" #-}
        if res == -1 then
            throwErrno "epoll_ctl(EPOLL_CTL_DEL) call failed"
        else
            return (EPoll epollfd (Map.delete fd fdmap))
    else
        error "del: fd not member of fdmap"

mod :: EPoll a -> CInt -> Word32 -> a ->  IO (EPoll a)
mod (EPoll epollfd fdmap) fd opts d = do
    if Map.member fd fdmap then do
        res <- alloca $ \ptr -> do
            poke ptr (EPollEvent opts fd)
            c_epoll_ctl epollfd (3) fd ptr
{-# LINE 170 "EPoll.hsc" #-}
        if res == -1 then
            throwErrno "epoll_ctl(EPOLL_CTL_MOD) call failed"
        else
            return (EPoll epollfd (Map.insert fd d fdmap))
    else
        error "mod: fd not member of fdmap"

