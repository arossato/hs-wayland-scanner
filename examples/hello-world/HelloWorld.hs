module Main where

import Foreign hiding ( void )
import Foreign.C.String

import Control.Monad
import System.Exit
import Data.IORef

import Graphics.Wayland.Protocol.Wayland
import Graphics.Wayland.Client.Core
import Graphics.Wayland.Client.Protocol.Wayland
import Graphics.Wayland.Client.Protocol.XdgShell

import Buffer

-- A simple state as user data
data State = State
  { display    :: Ptr WlDisplay
  , compositor :: Ptr WlCompositor
  , wlShm      :: Ptr WlShm
  , wmBase     :: Ptr XdgWmBase
  , surface    :: Ptr WlSurface
  }

-- xdg_wm_base ping: let the server know we are still alive
wmBasePingCb :: Ptr () -> Ptr XdgWmBase -> Word32 -> IO ()
wmBasePingCb _ wm serial = xdg_wm_base_pong wm serial

-- xdg_surface configure
configureCb :: Ptr () -> Ptr XdgSurface -> Word32 -> IO ()
configureCb dataPtr xdgSurf serial = do
  putStrLn "[CLIENT] Received xdg_surface.configure"
  stRef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  st <- readIORef stRef
  void $ xdg_surface_ack_configure xdgSurf serial

  -- draw after configure
  buffer <- createBuffer (wlShm st) 600 400

  void $ wl_surface_attach (surface st) buffer 1 1
  void $ wl_surface_damage (surface st) 0 0 600 400
  void $ wl_surface_commit (surface st)

  -- send the commit to the server
  void $ wl_display_flush (display st)
  return ()

-- global registry callback
globalCb :: WlRegistryGlobalCb
globalCb dataPtr registry name interface _ = do
  iface <- peekCString interface
  stRef <- deRefStablePtr (castPtrToStablePtr dataPtr)

  case iface of
    "wl_compositor" -> do
      comp <- wl_registry_bind registry name wl_compositor_interface 4
      surf <- wl_compositor_create_surface (castPtr comp)
      modifyIORef' stRef (\s -> s { compositor = castPtr comp, surface = surf })
      putStrLn "[CLIENT] Bound compositor"

    "wl_shm" -> do
      shm <- wl_registry_bind registry name wl_shm_interface 1
      modifyIORef' stRef (\s -> s { wlShm = castPtr shm })

    "xdg_wm_base" -> do
      wm <- wl_registry_bind registry name xdg_wm_base_interface 1
      modifyIORef' stRef (\s -> s { wmBase = castPtr wm })
      putStrLn "[CLIENT] Bound xdg_wm_base"

      -- add the ping listener
      pingFun <- mkXdgWmBasePingCb wmBasePingCb
      let wmListener = XdgWmBaseListener pingFun
      alloca $ \ptr -> do
        poke ptr wmListener
        void $ xdg_wm_base_add_listener (castPtr wm) ptr (castPtr dataPtr)
    _ -> return ()

main :: IO ()
main = do
  -- connect
  dpy <- wl_display_connect nullPtr
  when (dpy == nullPtr) $ do
    putStrLn "Failed to connect to Wayland."
    exitFailure
  putStrLn "Connected!"
  
  -- the state
  stRef <- newIORef State
    { display             = dpy
    , compositor          = nullPtr
    , wlShm               = nullPtr
    , wmBase              = nullPtr
    , surface             = nullPtr
    }
  sp <- newStablePtr stRef

  -- get the registry
  registry <- wl_display_get_registry dpy

  -- add the global listener
  cb <- mkWlRegistryGlobalCb globalCb
  let listener = WlRegistryListener
                 { wlRegistryGlobal       = cb
                 , wlRegistryGlobalRemove = nullFunPtr
                 }

  alloca $ \ptr -> do
    poke ptr listener
    void $ wl_registry_add_listener registry ptr (castStablePtrToPtr sp)
    void $ wl_display_roundtrip dpy

  st <- readIORef stRef
  xdgSurf <- xdg_wm_base_get_xdg_surface (wmBase st) (surface st)
  top     <- xdg_surface_get_toplevel xdgSurf

  -- identify your window
  withCString "hello world" $ \str -> do
      xdg_toplevel_set_title  top str
      xdg_toplevel_set_app_id top str

  -- configure listener
  cfgFun <- mkXdgSurfaceConfigureCb configureCb
  let surfListener = XdgSurfaceListener cfgFun
  alloca $ \ptr -> do
    poke ptr surfListener
    void $ xdg_surface_add_listener xdgSurf ptr (castStablePtrToPtr sp)

  void $ wl_surface_commit (surface st)
  -- send the commit to the server
  void $ wl_display_flush dpy

  forever $ do
    void $ wl_display_dispatch dpy
    void $ wl_display_flush dpy
