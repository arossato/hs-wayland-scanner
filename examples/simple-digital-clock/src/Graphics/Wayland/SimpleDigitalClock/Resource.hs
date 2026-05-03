------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.SimpleDigitalClock.Resource
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE in hs-wayland-scanner)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports some functions for managing allocated memory
-- and resources to be freed and destroyed on exit. Also see
-- 'Resource'.
------------------------------------------------------------------------
module Graphics.Wayland.SimpleDigitalClock.Resource where

import Data.IORef
import Foreign hiding ( void )

import Graphics.Wayland.SimpleDigitalClock.Types

-- | Store created 'FunPtr' for callbacks.
managedFunPtr :: CbFunPtr -> Resource
managedFunPtr (CbFunPtr fp) = Resource (castFunPtrToPtr fp) (freeHaskellFunPtr . castPtrToFunPtr)

-- | Store 'malloc' allecated memory 'Ptr'.
managedHeap :: Ptr a -> Resource
managedHeap p = Resource p free

-- | Store Wayland objects.
managedWl :: (Ptr a -> IO ()) -> Ptr a -> Resource
managedWl destroyer p = Resource p destroyer

-- | Helper to add a resource
addResource :: IORef State -> Resource -> IO ()
addResource ref res = modifyIORef' ref $ \s -> s {resources = res : resources s }

-- | Helper to add a listener.
addListenerResoure :: IORef State
                   -> (Ptr a -> IO ()) -- ^ interface destroyer
                   -> Ptr a            -- ^ interface Ptr
                   -> [CbFunPtr]       -- ^ list of callback FunPtrs
                   -> Ptr b            -- ^ listener Ptr (allocated with 'malloc')
                   -> IO ()
addListenerResoure stRef destructor ptr funPtrs mptr = do
  addResource stRef $ managedWl destructor ptr
  mapM_ (addResource stRef . managedFunPtr) funPtrs
  addResource stRef $ managedHeap   mptr

-- | Free all resources. To be called on shutdown.
cleanupAllResources :: IORef State -> IO ()
cleanupAllResources stRef = do
  rs <- resources <$> readIORef stRef
  mapM_ (\(Resource p fin) -> fin p) rs
  modifyIORef' stRef (\s -> s {resources = []})
