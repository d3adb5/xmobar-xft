------------------------------------------------------------------------------
-- |
-- Module: Xmobar.X11.CairoDraw
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Fri Sep 09, 2022 02:03
--
-- Drawing the xmobar contents using Cairo and Pango
--
--
------------------------------------------------------------------------------

module Xmobar.X11.CairoDraw (drawInPixmap) where

import Control.Monad.IO.Class
import Control.Monad.Reader

import Graphics.X11.Xlib hiding (Segment)
import GI.Cairo.Render.Types

import Xmobar.Run.Parsers (Segment)
import Xmobar.X11.Types
import Xmobar.X11.CairoSurface
-- import Xmobar.Text.Pango
import Xmobar.Config.Types

drawInPixmap :: Pixmap -> Dimension -> Dimension -> [[Segment]] -> X ()
drawInPixmap p w h s = do
  xconf <- ask
  let disp = display xconf
      scr = screenOfDisplay disp 0
      c = config xconf
      fi = fromIntegral
  liftIO $ withBitmapSurface disp p scr (fi w) (fi h) (renderSegments c s)


renderSegment :: String -> String -> Surface -> Segment -> IO ()
renderSegment _fg _bg _surface _segment = undefined

renderSegments :: Config -> [[Segment]] -> Surface -> IO ()
renderSegments conf segments surface = do
  let bg = bgColor conf
      fg = fgColor conf
  mapM_ (renderSegment fg bg surface) (concat segments)
