module Main where

import Control.Concurrent
import Control.Monad
import Foreign hiding ( void )
import Foreign.C.String
import System.Exit

import Graphics.Wayland.Client.Core
import Graphics.Wayland.Client.Protocol.Wayland

-- Global registry callback
globalCb :: WlRegistryGlobalCb
globalCb _ registry name interface version = do
  iface <- peekCString interface
  putStrLn ("[CLIENT] Global: " ++ iface ++ " (name=" ++ show name ++ ")")
  when (iface == "wl_compositor") $ do
    comp <- wl_registry_bind
            registry
            name
            wl_compositor_interface
            version

    putStrLn "[CLIENT] bound compositor"

    -- Call create_surface
    void $ wl_compositor_create_surface (castPtr comp)

main :: IO ()
main = do
  -- 1. Connect
  display <- wl_display_connect nullPtr
  when (display == nullPtr) $ do
    putStrLn "Failed to connect to Wayland."
    exitFailure
  putStrLn "Connected!"

  -- 2. Get the registry
  registry <- wl_display_get_registry display

  -- 3. Add the global listener
  cb <- mkWlRegistryGlobalCb globalCb
  let listener = WlRegistryListener
                 { wlRegistryGlobal       = cb
                 , wlRegistryGlobalRemove = nullFunPtr
                 }

  alloca $ \ptr -> do
    poke ptr listener
    void $ wl_registry_add_listener registry ptr nullPtr

  void $ wl_display_roundtrip display
  void $ wl_display_flush     display

  -- give the server some time to elaborate
  threadDelay 2000000

  -- 4. Cleanup and exit
  wl_display_disconnect display
  exitSuccess
