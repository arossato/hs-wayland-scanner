------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.SimpleDigitalClock.Cairo
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE in hs-wayland-scanner)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- 'GI.Cairo.Render.renderWith' uses 'ForeignPtr' with
-- 'FinalizerPtr'. Unfortunately the GC seems not to be able to fire
-- those finalizers, probably unaware of the real memory allocation on
-- the C side, thus producing a huge memory leak. We need to import
-- the cairo functions to manually create and destroy surfaces and
-- contexts.
------------------------------------------------------------------------
module Graphics.Wayland.SimpleDigitalClock.Cairo where

import Control.Exception          ( bracket )
import Control.Monad.Trans.Reader ( runReaderT )
import Foreign             hiding ( void )
import Foreign.C.Types

import qualified GI.Cairo.Render.Internal as GI
import Data.GI.Base.ManagedPtr ( newManagedPtr_ )

-- Phantom types so raw surface and context pointers stay distinct.
data RawSurface
data RawContext

foreign import ccall "cairo.h cairo_image_surface_create_for_data"
  c_surfaceCreate :: Ptr Word8 -> CInt -> CInt -> CInt -> CInt
                  -> IO (Ptr RawSurface)

foreign import ccall "cairo.h cairo_surface_destroy"
  c_surfaceDestroy :: Ptr RawSurface -> IO ()

foreign import ccall "cairo.h cairo_create"
  c_contextCreate :: Ptr RawSurface -> IO (Ptr RawContext)

foreign import ccall "cairo.h cairo_destroy"
  c_contextDestroy :: Ptr RawContext -> IO ()

-- | Drop-in replacement for 'GI.Cairo.Render.renderWith':
-- surface and context are both destroyed synchronously via 'bracket',
-- with no GI finalizer involved.
renderWithRaw
  :: Ptr Word8  -- ^ pixel buffer (the mmap'd shm memory)
  -> CInt       -- ^ cairo format (0 = CAIRO_FORMAT_ARGB32)
  -> CInt       -- ^ width
  -> CInt       -- ^ height
  -> CInt       -- ^ stride
  -> GI.Render ()
  -> IO ()
renderWithRaw pixels fmt w h stride action =
  bracket (c_surfaceCreate pixels fmt w h stride) c_surfaceDestroy $ \surf ->
  bracket (c_contextCreate surf)                  c_contextDestroy $ \rawCr -> do
  -- Build a GI Cairo wrapper with no finalizer around our manually-owned
  -- raw pointer so that nothing is left for GC.
  mp <- newManagedPtr_ (castPtr rawCr :: Ptr GI.Cairo)
  runReaderT (GI.runRender action) (GI.Cairo mp)
