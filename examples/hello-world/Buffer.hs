{-# LANGUAGE ForeignFunctionInterface #-}
module Buffer where

import Foreign
import Foreign.C.Types
import Control.Monad
import System.Posix.Types (Fd(..), COff(..))
import System.Posix.IO (closeFd)
import System.Posix.Files (setFdSize, stdFileMode)
import System.Posix.SharedMem

-- Using the mmap package would be nice but there are some portabilty
-- issues (see below), so we will use mmap from the standard C library.
-- import System.IO.MMap

import Graphics.Wayland.Protocol.Wayland
import Graphics.Wayland.Client.Protocol.Wayland

-- Use mmap from the Standard C library.
foreign import ccall "sys/mman.h mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr ())

-- Helper to get the CInt out of Fd
unFd :: Fd -> CInt
unFd (Fd n) = n

-- Create a buffer and  fill it with green pixels.
createBuffer :: Ptr WlShm -> Int -> Int -> IO (Ptr WlBuffer)
createBuffer shm width height = do
  let stride = width * 4
      size   = fromIntegral $ stride * height
      -- A unique name for the shared memory segment.
      shmName = "/hs-wayland-example-" ++ show width ++ "x" ++ show height

  -- Create a shared memory object, with shmReadWrite and shmCreate.
  fd <- shmOpen shmName (ShmOpenFlags True True False False) stdFileMode

  -- Resize the memory segment to the correct size.
  setFdSize fd size

  -- Map the memory into our process.
  -- mmapFilePtr from the mmap package requires the full path which
  -- may be different in different POSIX systems.
  -- (ptr, _, _, _) <- mmapFilePtr ("/dev/shm" ++ shmName) ReadWrite (Just (0, fromIntegral size))
  ptr <- c_mmap nullPtr (fromIntegral size) (1 .|. 2) 1 (unFd fd) 0

  -- Fill with blue with 50% transparency (0x800000FF)
  let pixels = castPtr ptr :: Ptr Word32
  forM_ [0 .. width * height - 1] $ \i ->
    pokeElemOff pixels i 0x800000ff

  -- Create Wayland objects.
  pool   <- wl_shm_create_pool shm (unFd fd) (fromIntegral size)
  buffer <- wl_shm_pool_create_buffer pool
                0
                (fromIntegral width)
                (fromIntegral height)
                (fromIntegral stride)
                WL_SHM_FORMAT_ARGB8888 -- 32-bit ARGB format

  -- Cleanup.
  shmUnlink shmName
  closeFd fd

  return buffer
