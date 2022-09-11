{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.X11.Cairo
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Thu Sep 08, 2022 01:25
--
--
-- Xlib Cairo Surface creation
--
------------------------------------------------------------------------------

module Xmobar.X11.CairoSurface (withXlibSurface
                               , withBitmapSurface
                               , setSurfaceDrawable) where

import Graphics.X11.Xlib.Types
import Graphics.X11.Types
import Graphics.X11.Xlib (defaultScreenOfDisplay)
import Graphics.Rendering.Cairo.Types
import qualified Graphics.Rendering.Cairo.Internal as Internal

import Foreign
import Foreign.C

#include <cairo/cairo-xlib.h>

foreign import ccall "cairo_xlib_surface_create"
   cSurfaceCreate :: Display -> Drawable -> Visual -> CInt -> CInt -> Ptr Surface

foreign import ccall "cairo_xlib_surface_create_for_bitmap"
   cBitmapCreate :: Display -> Pixmap -> Screen -> CInt -> CInt -> Ptr Surface

foreign import ccall "cairo_xlib_surface_set_drawable"
   cSetDrawable :: Ptr Surface -> Drawable -> CInt -> CInt -> ()

createXlibSurface :: Display -> Drawable -> Visual -> Int -> Int -> IO Surface
createXlibSurface d dr v w h =
  mkSurface $ cSurfaceCreate d dr v (fromIntegral w) (fromIntegral h)

withXlibSurface ::
  Display -> Drawable -> Visual -> Int -> Int -> (Surface -> IO a) -> IO a
withXlibSurface d dr v w h f = do
  surface <- createXlibSurface d dr v w h
  ret <- f surface
  Internal.surfaceDestroy surface
  return ret

createBitmapSurface :: Display -> Pixmap -> Screen -> Int -> Int -> IO Surface
createBitmapSurface d p s w h =
  mkSurface $ cBitmapCreate d p s (fromIntegral w) (fromIntegral h)

withBitmapSurface :: Display -> Pixmap -> Int -> Int -> (Surface -> IO a) -> IO a
withBitmapSurface d p w h f = do
  surface <- createBitmapSurface d p (defaultScreenOfDisplay d) w h
  ret <- f surface
  Internal.surfaceDestroy surface
  return ret

setSurfaceDrawable :: Surface -> Drawable -> Int -> Int -> IO ()
setSurfaceDrawable surface dr w h =
  withSurface surface $
    \s -> return $ cSetDrawable s dr (fromIntegral w) (fromIntegral h)
