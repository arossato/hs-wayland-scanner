------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.SimpleDigitalClock.Render
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE in hs-wayland-scanner)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports 'updatePoolText' which uses "GI.Cairo.Render"
-- for rendering text (date and time) and colors in the allocated
-- 'Buffer'. Colors are hard-coded. Text is vertically and orizontaly
-- centered.
------------------------------------------------------------------------
module Graphics.Wayland.SimpleDigitalClock.Render where

import Foreign hiding ( void )
import Foreign.C.Types

import GI.Cairo.Render hiding ( x, y)
import Graphics.Wayland.SimpleDigitalClock.Types

updatePoolText :: WlPool -> (String, String) -> IO ()
updatePoolText p (dateLine, timeLine) = do
  let buf = poolBuffer p
      raw = castPtr (bufferPtr buf) :: Ptr CUChar
      w   = fromIntegral (bufferWidth  p) :: Int
      h   = fromIntegral (bufferHeight p) :: Int
      s   = fromIntegral (bufferStride p) :: Int
  withImageSurfaceForData raw FormatARGB32 w h s $ \surf ->
      renderWith surf $ do
        -- Background
        setSourceRGBA 0.05 0.05 0.1 0.7
        setOperator OperatorSource
        paint

        -- Text style
        setSourceRGB 1 1 1
        selectFontFace "Noto Sans" FontSlantNormal FontWeightNormal
        setFontSize 16.0

        fe <- fontExtents
        let lineH   = fontExtentsHeight fe
            cx      = fromIntegral w / 2.0  -- horizontal center
            cy      = fromIntegral h / 2.0  -- vertical center
            gap     = 4.0                   -- extra px between lines
            -- Baseline of line 1 sits above center, line 2 below
            y1      = cy - gap / 2.0
            y2      = cy + lineH + gap / 2.0

        -- Helper: compute centered x given a string
        let centeredX str = do
              ex <- textExtents str
              -- xbearing accounts for left-side whitespace/offset
              return $ cx - (textExtentsWidth ex / 2.0) - textExtentsXbearing ex

        x1 <- centeredX dateLine
        moveTo x1 y1
        showText dateLine

        x2 <- centeredX timeLine
        moveTo x2 y2
        showText timeLine
