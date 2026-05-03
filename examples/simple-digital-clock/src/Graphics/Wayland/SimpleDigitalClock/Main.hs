------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.SimpleDigitalClock.Main
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE in hs-wayland-scanner)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports the 'main' entry point and the 'mainLoop'. The
-- 'mainloop' uses 'timeout' and 'threadWaitRead' to perform a
-- conditional block: the thread is blocked for a maximum of one
-- second unless events are received on the 'waylandFd".
------------------------------------------------------------------------
module Graphics.Wayland.SimpleDigitalClock.Main where

import Control.Concurrent ( threadWaitRead )
import Control.Monad
import Data.IORef
import Foreign hiding ( void )
import Foreign.C.String
import System.Exit
import System.Timeout
import System.Posix.Types ( Fd(..) )

import Graphics.Wayland.Client.Core
import Graphics.Wayland.Client.Protocol.Wayland
import Graphics.Wayland.Client.Protocol.XdgShell

import Graphics.Wayland.SimpleDigitalClock.BufferPool
import Graphics.Wayland.SimpleDigitalClock.Listeners
import Graphics.Wayland.SimpleDigitalClock.Resource
import Graphics.Wayland.SimpleDigitalClock.Types

-- | Main entry point.
main :: IO ()
main = do
  -- connect
  dpy <- wl_display_connect nullPtr
  when (dpy == nullPtr) $ do
    putStrLn "[CLIENT] Failed to connect to Wayland."
    exitFailure
  putStrLn "[CLIENT] Connected!"

  wFd <- Fd <$> wl_display_get_fd dpy

  -- the state
  stRef <- newIORef initState {display = dpy, waylandFd = wFd}
  sp    <- newStablePtr stRef
  modifyIORef' stRef $ \s -> s { statePtr = castStablePtrToPtr sp }

  -- add the global listener
  setupRegistryListener stRef

  st <- readIORef stRef
  -- check we bound needed interfaces
  when (compositor st == nullPtr ||
        wlShm      st == nullPtr ||
        xdgWmBase  st == nullPtr ||
        wlDeco     st == nullPtr) $ do
    putStrLn "[CLIENT] Cannot bind needed interfaces. Exiting..."
    exitFailure

  -- create xdg_toplevel
  xdgSurf  <- xdg_wm_base_get_xdg_surface (xdgWmBase st) (wlSurface st)
  topLevel <- xdg_surface_get_toplevel xdgSurf
  modifyIORef' stRef $ \s -> s { xdgSurface  = xdgSurf
                               , xdgTopLevel = topLevel }

  -- identify our window
  withCString "Simple Digital Clock" $ \str -> do
      xdg_toplevel_set_title  topLevel str
      xdg_toplevel_set_app_id topLevel str

  -- add the ping listener
  setupWmBaseListener stRef

  -- add xdg toplevel listener
  setupXdgToplevelListener stRef

  -- configure decoration listener
  setupZxdgToplevelDecorationListener stRef

  -- add surface listener
  setupXdgSurfaceListener stRef

  -- add frame callback listener
  setupWlCallbackListener stRef

  -- commit
  void $ wl_surface_commit (wlSurface st)
  -- send the commit to the server
  void $ wl_display_flush dpy
  -- dispatch incoming events
  void $ wl_display_dispatch (display st)
  -- block till the compositor processed all requests
  void $ wl_display_roundtrip (display st)
  -- start the main loop
  mainLoop stRef

-- | 'mainLoop': after updating block for one second with
-- 'threadWaitRead' unless events are being received through the
-- 'waylandFd'.
mainLoop :: IORef State -> IO ()
mainLoop stRef = do
  st <- readIORef stRef
  when (running st) $ do

    -- 1. Flush any outgoing requests
    void $ wl_display_flush (display st)

    -- 2. Check our state
    t <- getTime
    let pa = poolA st
        pb = poolB st
        w  = pendingWidth  st
        h  = pendingHeight st
    let timeChanged = t /= lastTime st
        sizeChanged = w /= bufferWidth  pa ||
                      h /= bufferHeight pa ||
                      w /= bufferWidth  pb ||
                      h /= bufferHeight pb
    modifyIORef' stRef $ \s -> s { lastTime = t }
    -- 3. Draw only if necessary
    unless (sleeping st) $ do
      when (timeChanged || sizeChanged) $ do
        resizeAndUpdate stRef w h
        modifyIORef' stRef $ \s -> s { sleeping = True }
        nextCb <- wl_surface_frame (wlSurface st)
        void $ wl_callback_add_listener nextCb (cbListener st) (statePtr st)
        void $ wl_surface_commit (wlSurface st)
        void $ wl_display_flush  (display   st)

    -- 4. Wait for data OR the 1-second timer
    -- Just () means data is ready; Nothing means 1s passed.
    eventReady <- timeout 1000000 (threadWaitRead $ waylandFd st)

    -- 5. Dispatch ONLY if there is actually data to read.
    -- If eventReady is Nothing, wl_display_dispatch would hang!
    case eventReady of
        Just _ ->
            -- Data is on the wire. Read and process it.
            void $ wl_display_dispatch (display st)
        Nothing ->
            -- Timer expired, socket is empty.
            -- Just process anything already in the internal queue.
            void $ wl_display_dispatch_pending (display st)

    mainLoop stRef

  putStrLn "[CLIENT] Cleaning up resources..."
  cleanupAllResources stRef
  destroyPool (poolA st)
  destroyPool (poolB st)
  -- Disconnect from the Wayland display socket
  wl_display_disconnect (display st)
  -- Finally, release the stable pointer
  freeStablePtr (castPtrToStablePtr $ statePtr st)
  -- exit
  putStrLn "[CLIENT] Shutdown complete. Exiting..."
  exitSuccess
