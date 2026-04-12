module Main where

import Control.Monad
import Foreign hiding ( void )
import Foreign.C

import Graphics.Wayland.Server.Core
import Graphics.Wayland.Server.Protocol.Wayland

-- Request callback: wl_compositor_create_surface
createSurface :: WlCompositorCreateSurfaceCb
createSurface _client _resource newId =
  putStrLn ("[SERVER] create_surface called, new id = " ++ show newId)

-- Interface struct
compositorInterface :: IO WlCompositorInterface
compositorInterface = do
  cs <- mkWlCompositorCreateSurfaceCb createSurface
  return WlCompositorInterface
    { wlCompositorCreateSurface = cs
    , wlCompositorCreateRegion  = nullFunPtr
    }

-- Global bind callback
bindImpl :: WlGlobalBindFuncCb
bindImpl client _ _ newId = do
  putStrLn "[SERVER] Client bound to wl_compositor"

  resource <- wl_resource_create
                client
                wl_compositor_interface
                1
                newId

  alloca $ \ptr -> do
    poke ptr =<< compositorInterface
    wl_resource_set_implementation
      resource
      (castPtr ptr)  -- pointer to compositorInterface
      nullPtr
      nullFunPtr

main :: IO ()
main = do
  -- create the display
  display <- wl_display_create

  bindCb <- mkWlGlobalBindFuncCb bindImpl

  void $ wl_global_create
           display
           wl_compositor_interface
           1
           nullPtr
           bindCb

  namePtr <- wl_display_add_socket_auto display
  name    <- peekCString namePtr

  putStrLn   "[SERVER] Wayland server running..."
  putStrLn $ "[SERVER] Socket: " ++ name
  
  wl_display_run display
