------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.SimpleDigitalClock.Listeners
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE in hs-wayland-scanner)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module implements various listeners (event handlers) in terms
-- of callback functions..
------------------------------------------------------------------------
module Graphics.Wayland.SimpleDigitalClock.Listeners where

import Control.Monad
import Data.IORef
import Foreign hiding ( void )
import Foreign.C.String

import Graphics.Wayland.Protocol.Wayland
import Graphics.Wayland.Client.Core
import Graphics.Wayland.Client.Protocol.Wayland
import Graphics.Wayland.Client.Protocol.XdgShell
import Graphics.Wayland.Protocol.XdgDecorationUnstableV1
import Graphics.Wayland.Client.Protocol.XdgDecorationUnstableV1

import Graphics.Wayland.SimpleDigitalClock.BufferPool
import Graphics.Wayland.SimpleDigitalClock.Resource
import Graphics.Wayland.SimpleDigitalClock.Types

--------------------------------------------------------------------------------
--
-- * Wayland Core Protocol Listeners
--
-- $core
--------------------------------------------------------------------------------

-- | Global registry callback ('WlRegistryGlobalCb')
globalRegistryCb :: WlRegistryGlobalCb
globalRegistryCb stPtr registry name interface _ = do
  iface <- peekCString interface
  stRef <- deRefStablePtr (castPtrToStablePtr stPtr)

  case iface of
    "wl_compositor" -> do
      comp <- wl_registry_bind registry name wl_compositor_interface 4
      surf <- wl_compositor_create_surface (castPtr comp)
      modifyIORef' stRef $ \s -> s { compositor = castPtr comp, wlSurface = surf }
      addResource stRef $ managedWl wl_surface_destroy surf
      addResource stRef $ managedWl wl_compositor_destroy (castPtr comp)
      putStrLn "[CLIENT] Bound compositor"

    "wl_shm" -> do
      shm <- wl_registry_bind registry name wl_shm_interface 1
      modifyIORef' stRef $ \s -> s { wlShm = castPtr shm }
      putStrLn "[CLIENT] Bound wl_shm"
      addResource stRef $ managedWl wl_shm_destroy (castPtr shm)

    "xdg_wm_base" -> do
      wm <- wl_registry_bind registry name xdg_wm_base_interface 1
      modifyIORef' stRef $ \s -> s { xdgWmBase = castPtr wm }
      putStrLn "[CLIENT] Bound xdg_wm_base"
      addResource stRef $ managedWl xdg_wm_base_destroy (castPtr wm)

    "zxdg_decoration_manager_v1" -> do
      deco <- wl_registry_bind registry name zxdg_decoration_manager_v1_interface 1
      modifyIORef' stRef $ \s -> s { wlDeco = castPtr deco }
      putStrLn "[CLIENT] Bound zxdg_decoration_manager_v1"
      addResource stRef $ managedWl zxdg_decoration_manager_v1_destroy (castPtr deco)

    _ -> return ()

-- | Add the global listener.
setupRegistryListener :: IORef State -> IO ()
setupRegistryListener stRef = do
  st <- readIORef stRef
  -- get the registry
  registry <- wl_display_get_registry (display st)

  -- add the global listener
  cb <- mkWlRegistryGlobalCb globalRegistryCb
  let listener = WlRegistryListener
                 { wlRegistryGlobal       = cb
                 , wlRegistryGlobalRemove = nullFunPtr
                 }
  malloc >>= \ptr -> do
    poke ptr listener
    void $ wl_registry_add_listener registry ptr (statePtr st)
    void $ wl_display_roundtrip (display st)
    addListenerResoure stRef wl_registry_destroy registry [CbFunPtr cb] ptr

-- | Add the 'WlCallbackListener' to get the 'wlCallbackDone' event:
-- when we do not receive this event our client is minimized or the
-- screen server is active, so we go to sleep. See 'mainLoop'.
wlCallbackDoneCb :: Ptr () -> Ptr WlCallback -> Word32 -> IO ()
wlCallbackDoneCb stPtr cb w = do
  stRef <- deRefStablePtr (castPtrToStablePtr stPtr)
  st    <- readIORef stRef
  when (debug st) $ putStrLn $ "[DEBUG] WlCallbackDone received: " ++ show w
  wl_callback_destroy cb
  modifyIORef' stRef $ \s -> s { sleeping = False }

-- | Add the 'WlCallbackListener'.
setupWlCallbackListener :: IORef State -> IO ()
setupWlCallbackListener stRef = do
  st            <- readIORef stRef
  frameCallback <- wl_surface_frame (wlSurface st)
  cbFun <- mkWlCallbackDoneCb wlCallbackDoneCb
  let callbackListener = WlCallbackListener cbFun
  malloc >>= \ptr -> do
    poke ptr callbackListener
    void $ wl_callback_add_listener frameCallback ptr (statePtr st)
    -- we are going to reuse this
    modifyIORef' stRef $ \s -> s { cbListener = ptr }
    addResource stRef $ managedHeap ptr
    addResource stRef $ managedFunPtr $ CbFunPtr cbFun

--------------------------------------------------------------------------------
--
-- * Xdg-Shell Protocol Listeners
--
-- $xdg
--------------------------------------------------------------------------------

-- | 'xdgWmBasePing': let the server know we are still alive.
wmBasePingCb :: Ptr () -> Ptr XdgWmBase -> Word32 -> IO ()
wmBasePingCb _ = xdg_wm_base_pong

-- | Add the 'XdgWmBaseListener'.
setupWmBaseListener :: IORef State -> IO ()
setupWmBaseListener stRef = do
  st <- readIORef stRef
  pingFun <- mkXdgWmBasePingCb wmBasePingCb
  let wmListener = XdgWmBaseListener pingFun
  malloc >>= \ptr -> do
    poke ptr wmListener
    void $ xdg_wm_base_add_listener (xdgWmBase st) ptr (statePtr st)
    addListenerResoure stRef xdg_wm_base_destroy (xdgWmBase st) [CbFunPtr pingFun] ptr

-- | Implement the 'xdgToplevelConfigure' event: this event will let
-- the client know the available window's size. When we receive this
-- event we set the window's size and we create the needed
-- 'WlPool's. See the "Graphics.Wayland.SimpleDigitalClock.BufferPool"
-- module.
xdgToplevelConfigureCb :: Ptr () -> Ptr XdgToplevel -> Int32 -> Int32 -> Ptr WlArray -> IO ()
xdgToplevelConfigureCb stPtr _ width height _ = do
  stRef <- deRefStablePtr (castPtrToStablePtr stPtr)
  st    <- readIORef stRef
  when (debug st) $
    putStrLn $ "[DEBUG] xdg_toplevel.configure: " ++ show width ++ "x" ++ show height
  -- If width/height are 0, the compositor is letting you pick the size.
  -- otherwise use the dimensions provided.
  let w = if width  == 0 then 600 else width
      h = if height == 0 then 400 else height

  pa <- if wlBuffer (poolBuffer $ poolA st) == nullPtr
          -- we need to create the pool
        then createPool stPtr (wlShm st) w h
        else return $ poolA st
  pb <- if wlBuffer (poolBuffer $ poolB st) == nullPtr
          -- we need to create the pool
        then createPool stPtr (wlShm st) w h
        else return $ poolB st
  modifyIORef' stRef $ \s -> s { pendingWidth  = w
                               , pendingHeight = h
                               , poolA = pa
                               , poolB = pb}

-- | Implement the 'XdgToplevelCloseCb' callback. The event is
-- received when the user wants to close the window.
xdgToplevelCloseCb :: Ptr () -> Ptr XdgToplevel -> IO ()
xdgToplevelCloseCb stPtr _ = do
  stRef <- deRefStablePtr (castPtrToStablePtr stPtr)
  st    <- readIORef stRef
  when (debug st) $
    putStrLn "[DEBUG] xdg_toplevel.close received. Exiting."
  modifyIORef' stRef $ \s -> s { running = False }

-- | Add the 'XdgToplevelListener'.
setupXdgToplevelListener :: IORef State -> IO ()
setupXdgToplevelListener stRef = do
  st <- readIORef stRef
  topCfgFun <- mkXdgToplevelConfigureCb xdgToplevelConfigureCb
  topClsFun <- mkXdgToplevelCloseCb     xdgToplevelCloseCb
  let topListener = XdgToplevelListener topCfgFun topClsFun nullFunPtr nullFunPtr
  malloc >>= \ptr -> do
    poke ptr topListener
    void $ xdg_toplevel_add_listener (xdgTopLevel st) ptr (statePtr st)
    addListenerResoure stRef xdg_toplevel_destroy (xdgTopLevel st) [CbFunPtr topCfgFun, CbFunPtr topClsFun] ptr

-- | Implement the 'xdgSurfaceConfigure' callback. We this event we
-- actually update our unused 'Buffer's and commit it.
xdgSurfaceConfigureCb :: Ptr () -> Ptr XdgSurface -> Word32 -> IO ()
xdgSurfaceConfigureCb stPtr xdgSurf serial = do
  stRef <- deRefStablePtr (castPtrToStablePtr stPtr)
  st    <- readIORef stRef
  when(debug st) $
    putStrLn $ "[DEBUG] Received xdg_surface.configure. Serial = " ++ show serial
  -- Ack the configure
  void $ xdg_surface_ack_configure xdgSurf serial
  let w = pendingWidth  st
      h = pendingHeight st
  resizeAndUpdate stRef w h
  void $ wl_surface_damage (wlSurface st) 0 0 w h
  void $ wl_surface_commit (wlSurface st)

-- | Add the 'XdgSurfaceListener'
setupXdgSurfaceListener :: IORef State -> IO ()
setupXdgSurfaceListener stRef = do
  st     <- readIORef stRef
  cfgFun <- mkXdgSurfaceConfigureCb xdgSurfaceConfigureCb
  let surfListener = XdgSurfaceListener cfgFun
  malloc >>= \ptr -> do
    poke ptr surfListener
    void $ xdg_surface_add_listener (xdgSurface st) ptr (statePtr st)
    addListenerResoure stRef xdg_surface_destroy (xdgSurface st) [CbFunPtr cfgFun] ptr

--------------------------------------------------------------------------------
--
-- * Xdg-Decoration Protocol Listeners
--
-- $xdg-deco
--------------------------------------------------------------------------------

-- | Implement the 'zxdgToplevelDecorationV1Configure' callback in
-- order to receive the available decoration modes supported by the
-- compositor.
xdgToplevelDecorationV1ConfigureCb :: Ptr () -> Ptr ZxdgToplevelDecorationV1 -> ZXDG_TOPLEVEL_DECORATION_V1_MODE -> IO ()
xdgToplevelDecorationV1ConfigureCb stPtr deco mode = do
  stRef <- deRefStablePtr (castPtrToStablePtr stPtr)
  st    <- readIORef stRef
  when (debug st) $ do
    putStrLn "[DEBUG] Received toplevel_decoration.configure"
    case mode of
      0 -> putStrLn   "[DEBUG] Server says: CLIENT_SIDE (You must draw!)"
      1 -> putStrLn   "[DEBUG] Server says: SERVER_SIDE (Compositor draws!)"
      _ -> putStrLn $ "[DEBUG] Server sent unknown mode: " ++ show mode
  modifyIORef' stRef $ \s -> s { topLevelDeco = deco }

-- | Add the 'ZxdgToplevelDecorationV1Listener'.
setupZxdgToplevelDecorationListener :: IORef State -> IO ()
setupZxdgToplevelDecorationListener stRef = do
  st   <- readIORef stRef
  unless (wlDeco st == nullPtr) $ do
    deco <- zxdg_decoration_manager_v1_get_toplevel_decoration (wlDeco st) (xdgTopLevel st)
    void $ zxdg_toplevel_decoration_v1_set_mode deco ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE
    cfgDeco <- mkZxdgToplevelDecorationV1ConfigureCb xdgToplevelDecorationV1ConfigureCb
    let decoListener = ZxdgToplevelDecorationV1Listener cfgDeco
    malloc >>= \ptr -> do
      poke ptr decoListener
      void $ zxdg_toplevel_decoration_v1_add_listener deco ptr (statePtr st)
      addListenerResoure stRef zxdg_toplevel_decoration_v1_destroy deco [CbFunPtr cfgDeco] ptr
