{-# LANGUAGE ForeignFunctionInterface #-}
------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.SimpleDigitalClock.BufferPool
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE in hs-wayland-scanner)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports functions for managing 'WlPool's and
-- 'Buffer's. In this client we use double-buffering with two distinct
-- 'WlPool'. This is due to the fact that supporting resizing with a
-- single 'WlPool' and two 'Buffer's would make size changes much more
-- complicated.
------------------------------------------------------------------------
module Graphics.Wayland.SimpleDigitalClock.BufferPool where

import Control.Monad
import Data.IORef
import Foreign hiding ( void, newPool )
import Foreign.C.Types
import System.Posix.Types (Fd(..), COff(..))
import System.Posix.IO (closeFd)
import System.Posix.Files (setFdSize, stdFileMode)
import System.Posix.SharedMem

import Graphics.Wayland.Protocol.Wayland
import Graphics.Wayland.Client.Protocol.Wayland

import GI.Cairo.Render (formatStrideForWidth, Format(..))

import Data.Time

import Graphics.Wayland.SimpleDigitalClock.Types
import Graphics.Wayland.SimpleDigitalClock.Render

-- | Use mmap from the Standard C library.
foreign import ccall "sys/mman.h mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr ())

-- | Use munmap from the Standard C library.
foreign import ccall "sys/mman.h munmap"
  c_munmap :: Ptr () -> CSize -> IO CInt

-- | Helper to get the CInt out of Fd
unFd :: Fd -> CInt
unFd (Fd n) = n

--------------------------------------------------------------------------------
--
-- * WlPool managmenet
--
-- $pool
--------------------------------------------------------------------------------

-- | Create a new 'WlPool'.
createPool :: Ptr () -> Ptr WlShm -> Int32 -> Int32 -> IO WlPool
createPool dataPtr shm width height = do
  let (size, stride) = getSizeAndStride width height
      shmName = "/hs-clock-shm"
  fd <- shmOpen shmName (ShmOpenFlags True True False False) stdFileMode
  setFdSize fd (fi size)
  -- Map the memory
  ptr <- c_mmap nullPtr (fi size) (1 .|. 2) 1 (unFd fd) 0
  -- Create Wayland side
  pool   <- wl_shm_create_pool shm (unFd fd) size
  buffer <- createBuffer dataPtr ptr pool  (fi width) (fi height) (fi stride)
  -- Clean up names (FD stays open in our record)
  shmUnlink shmName
  return $ WlPool 
    { wlPool       = pool
    , poolFd       = fd
    , wlShmPtr     = shm
    , userData     = dataPtr
    , poolSize     = size
    , bufferWidth  = width
    , bufferHeight = height
    , bufferStride = stride
    , poolBuffer   = buffer
    , poolIsBusy   = False
    }

-- | Used only to increase the 'Wlpool' size.
resizePool :: WlPool -> Int32 -> Int32 -> IO WlPool
resizePool pool newWidth newHeight = do
  stRef <- deRefStablePtr (castPtrToStablePtr $ userData pool)
  st    <- readIORef stRef
  let (size, stride) = getSizeAndStride newWidth newHeight
  when (debug st) $
    putStrLn $ "[DEBUG]: resize. Pool Size: " ++ show (poolSize pool) ++ " New Buffer Req: " ++ show size
  setFdSize (poolFd pool) (fi size)
  wl_shm_pool_resize (wlPool pool) size
  void $ c_munmap (bufferPtr $ poolBuffer pool) (fi $ poolSize pool)
  newPtr <- c_mmap nullPtr (fi size) (1 .|. 2) 1 (unFd $ poolFd pool) 0
  buffer <- destroyBuffer st (poolBuffer pool) >>
            createBuffer (userData pool) newPtr (wlPool pool) newWidth newHeight stride
  return $ pool { poolSize     = size
                , bufferWidth  = newWidth
                , bufferHeight = newHeight
                , bufferStride = stride
                , poolBuffer   = buffer
                }

-- | It would be more efficient to shrink only the 'Buffer', unless
-- its size decreased enough that it would be convenient to free the
-- unused memory. Instead, for simplicity, we just recreate the
-- 'WlPool'.
shrinkPool :: WlPool -> Int32 -> Int32 -> IO WlPool
shrinkPool = recreatePool

-- | Destroy the 'WlPool', free all memory and close the file
-- descriptor.
destroyPool :: WlPool -> IO ()
destroyPool p = do
  stRef <- deRefStablePtr (castPtrToStablePtr $ userData p)
  st    <- readIORef stRef
  when (debug st) $
    putStrLn "[DEBUG] Destroying the WlPool"
  destroyBuffer st (poolBuffer p)
  void $ c_munmap (bufferPtr $ poolBuffer p) (fi $ poolSize p)
  wl_shm_pool_destroy (wlPool p)
  closeFd (poolFd p)

recreatePool :: WlPool -> Int32 -> Int32 -> IO WlPool
recreatePool p newWidth newHeight = do
  destroyPool p
  createPool (userData p) (wlShmPtr p) newWidth newHeight

--------------------------------------------------------------------------------
--
-- * Buffer managmenet
--
-- $buffers
--------------------------------------------------------------------------------

-- | Create a new 'Buffer', adding a 'WlBufferListener' to get the
-- 'wlBufferRelease' event.
createBuffer :: Ptr () -> Ptr () -> Ptr WlShmPool -> Int32 -> Int32 -> Int32 -> IO Buffer
createBuffer stPtr bufPtr poolPtr w h s = do
  stRef <- deRefStablePtr (castPtrToStablePtr stPtr)
  st    <- readIORef stRef
  buf   <- wl_shm_pool_create_buffer poolPtr 0 w h s WL_SHM_FORMAT_ARGB8888
  when (debug st) $
    putStrLn $ "[DEBUG]  buffer created with " ++ show (bufPtr, w, h, s)
  -- create the listener
  bufRelease <- mkWlBufferReleaseCb bufferReleaseCb
  let bufRelListener = WlBufferListener bufRelease
  malloc >>= \ptr -> do
    poke ptr bufRelListener
    void $ wl_buffer_add_listener buf ptr stPtr
    return $ Buffer
      { wlBuffer       = buf
      , bufferPtr      = bufPtr
      , bufferRelease  = bufRelease
      , bufferListener = ptr
      }

-- | Destroy the 'Buffer' and free all allocated memory.
destroyBuffer :: State -> Buffer -> IO ()
destroyBuffer st b = do
  when (debug st) $
    putStrLn $ "[DEBUG] Actually destroying buffer: " ++ show (wlBuffer b)
  wl_buffer_destroy (wlBuffer       b)
  free              (bufferListener b)
  freeHaskellFunPtr (bufferRelease  b)

-- | Buffer release callback ('wlBufferRelease').
bufferReleaseCb :: Ptr () -> Ptr WlBuffer -> IO ()
bufferReleaseCb dataPtr buf = do
  stRef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  st    <- readIORef stRef
  when (debug st) $
    putStrLn $ "[DEBUG] Buffer released by compositor: " ++ show buf
  let pa = poolA st
      pb = poolB st
  when (wlBuffer (poolBuffer pa) == buf) $
    modifyIORef' stRef $ \s -> s { poolA = unsetIsBusy pa }
  when (wlBuffer (poolBuffer pb) == buf) $
    modifyIORef' stRef $ \s -> s { poolB = unsetIsBusy pb }

-- | Resize the 'Buffer' and update the displayed time.
resizeAndUpdate :: IORef State -> Int32 -> Int32 -> IO ()
resizeAndUpdate stRef w h = do
  st <- readIORef stRef
  let pa = poolA st
      pb = poolB st
      oldSizeA = poolSize pa
      oldSizeB = poolSize pb
      newSize  = fst $ getSizeAndStride w h
      resize p s = case compare newSize s of
                     GT -> resizePool p w h
                     LT -> shrinkPool p w h
                     _  -> return p
      updateClock p = do
        time <- getTime
        modifyIORef' stRef $ \s -> s { lastTime = time }
        updatePoolText p time
        void $ wl_surface_attach (wlSurface st) (wlBuffer $ poolBuffer p) 0 0

  case (poolIsBusy pa, poolIsBusy pb) of
    (True,  True ) -> when (debug st) $ putStrLn "[DEBUG] both busy"
    (False, False) -> do
      when (debug st) $
        putStrLn $ "[DEBUG] resizing both " ++ show (w,h)
      pa' <- resize pa oldSizeA
      pb' <- resize pb oldSizeB
      modifyIORef' stRef $ \s -> s { poolA = setIsBusy pa'
                                   , poolB = pb'}
      updateClock pa'
    (False,     _) -> do
      when (debug st) $
        putStrLn $ "[DEBUG] resizing A: " ++ show (oldSizeA, newSize, w,h)
      pa' <- resize pa oldSizeA
      modifyIORef' stRef $ \s -> s { poolA = setIsBusy pa'}
      updateClock pa'
    (_    , False) -> do
      when (debug st) $
        putStrLn $ "[DEBUG] resizing B: "  ++ show (oldSizeB, newSize, w,h)
      pb' <- resize pb oldSizeB
      modifyIORef' stRef $ \s -> s { poolB = setIsBusy pb'}
      updateClock pb'

--------------------------------------------------------------------------------
--
-- * Helpers
--
-- $helpers
--------------------------------------------------------------------------------

getTime :: IO (String, String)
getTime = do
  now <- getZonedTime
  let dateLine = formatTime defaultTimeLocale "%a %b %d %Y" now
      timeLine = formatTime defaultTimeLocale "%H:%M:%S"    now
  return (dateLine, timeLine)


fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

setIsBusy :: WlPool -> WlPool
setIsBusy p = p { poolIsBusy = True }

unsetIsBusy :: WlPool -> WlPool
unsetIsBusy p = p { poolIsBusy = False }

getSizeAndStride :: Int32 -> Int32 -> (Int32, Int32)
getSizeAndStride width height =
  let stride = fi $ formatStrideForWidth FormatARGB32 $ fi width
      size   = fi $ stride * height
  in (size, stride)
