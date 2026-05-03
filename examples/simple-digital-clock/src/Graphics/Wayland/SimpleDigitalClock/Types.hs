{-# LANGUAGE ExistentialQuantification #-}
------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.SimpleDigitalClock.Types
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE in hs-wayland-scanner)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports the 'State' of the client and some other
-- datatypes.
------------------------------------------------------------------------
module Graphics.Wayland.SimpleDigitalClock.Types where

import Foreign hiding ( void )
import System.Posix.Types ( Fd(..) )

import Graphics.Wayland.Protocol.Wayland
import Graphics.Wayland.Client.Protocol.Wayland
import Graphics.Wayland.Client.Protocol.XdgShell
import Graphics.Wayland.Client.Protocol.XdgDecorationUnstableV1

-- | A simple state passed as user data.
data State = State
  { display        :: Ptr WlDisplay
  , waylandFd      :: Fd
  , statePtr       :: Ptr ()
  , compositor     :: Ptr WlCompositor
  , wlShm          :: Ptr WlShm
  , xdgWmBase      :: Ptr XdgWmBase
  , wlSurface      :: Ptr WlSurface
  , xdgSurface     :: Ptr XdgSurface
  , wlDeco         :: Ptr ZxdgDecorationManagerV1
  , xdgTopLevel    :: Ptr XdgToplevel
  , topLevelDeco   :: Ptr ZxdgToplevelDecorationV1
  , poolA          :: WlPool
  , poolB          :: WlPool
  , pendingWidth   :: Int32
  , pendingHeight  :: Int32
  , sleeping       :: Bool
  , running        :: Bool
  , resources      :: [Resource]
  , lastTime       :: (String, String)
  , cbListener     :: Ptr WlCallbackListener
  , debug          :: Bool
  }

-- | The inital 'State'
initState :: State
initState = State
  { display        = nullPtr
  , waylandFd      = Fd 0
  , statePtr       = nullPtr
  , compositor     = nullPtr
  , wlShm          = nullPtr
  , xdgWmBase      = nullPtr
  , wlSurface      = nullPtr
  , xdgSurface     = nullPtr
  , wlDeco         = nullPtr
  , xdgTopLevel    = nullPtr
  , topLevelDeco   = nullPtr
  , poolA          = nullPool
  , poolB          = nullPool
  , pendingWidth   = 0
  , pendingHeight  = 0
  , sleeping       = False
  , running        = True
  , resources      = []
  , lastTime       = ("", "")
  , cbListener     = nullPtr
  , debug          = False
  }

-- | A self-contained cleanup action wrapped in an existential
-- datatype.
data Resource = forall a . Resource
  { rPtr     :: Ptr a
  , rCleanup :: Ptr a -> IO ()
  }

-- | An existential datatype for storing callback 'FunPtr'.
data CbFunPtr = forall a . CbFunPtr (FunPtr a)

-- | The 'WlPool' datatype stores information about created 'WlPool's.
data WlPool = WlPool
  { wlPool       :: Ptr WlShmPool
  , poolFd       :: Fd
  , wlShmPtr     :: Ptr WlShm
  , userData     :: Ptr ()
  , poolSize     :: Int32
  , bufferWidth  :: Int32
  , bufferHeight :: Int32
  , bufferStride :: Int32
  , poolBuffer   :: Buffer
  , poolIsBusy   :: Bool
  } deriving Show

-- | The 'Buffer' datatype stores information about created 'Buffer's.
data Buffer = Buffer
  { wlBuffer       :: Ptr WlBuffer
  , bufferPtr      :: Ptr ()
  , bufferRelease  :: FunPtr WlBufferReleaseCb
  , bufferListener :: Ptr WlBufferListener
  } deriving Show

nullPool :: WlPool
nullPool = WlPool nullPtr 0 nullPtr nullPtr 0 0 0 0 nullBuffer False

nullBuffer :: Buffer
nullBuffer = Buffer nullPtr nullPtr nullFunPtr nullPtr
