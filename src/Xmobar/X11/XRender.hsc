------------------------------------------------------------------------------
-- |
-- Module: Xmobar.X11.XRender
-- Copyright: (c) 2012, 2014, 2015, 2017, 2022 Jose Antonio Ortega Ruiz
--            (c) Clemens Fruhwirth <clemens@endorphin.org> 2007
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Sun Sep 11, 2022 01:27
--
--
-- A couple of utilities imported from libxrender to allow alpha blending of
-- an image backgrond.
--
------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Xmobar.X11.XRender (drawBackground) where

import Graphics.X11
import Graphics.X11.Xrender
import Graphics.X11.Xlib.Extras (xGetWindowProperty, xFree)
import Control.Monad (when)

import Foreign
import Foreign.C.Types

#include <X11/extensions/Xrender.h>

type Picture = XID
type PictOp = CInt

data XRenderPictFormat
data XRenderPictureAttributes = XRenderPictureAttributes

-- foreign import ccall unsafe "X11/extensions/Xrender.h XRenderFillRectangle"
-- xRenderFillRectangle :: Display -> PictOp -> Picture -> Ptr XRenderColor -> CInt -> CInt -> CUInt -> CUInt -> IO ()
foreign import ccall unsafe "X11/extensions/Xrender.h XRenderComposite"
  xRenderComposite :: Display -> PictOp -> Picture -> Picture -> Picture -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CUInt -> CUInt -> IO ()
foreign import ccall unsafe "X11/extensions/Xrender.h XRenderCreateSolidFill"
  xRenderCreateSolidFill :: Display -> Ptr XRenderColor -> IO Picture
foreign import ccall unsafe "X11/extensions/Xrender.h XRenderFreePicture"
  xRenderFreePicture :: Display -> Picture -> IO ()
foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()
foreign import ccall unsafe "X11/extensions/Xrender.h XRenderFindStandardFormat"
  xRenderFindStandardFormat :: Display -> CInt -> IO (Ptr XRenderPictFormat)
foreign import ccall unsafe "X11/extensions/Xrender.h XRenderCreatePicture"
  xRenderCreatePicture :: Display -> Drawable -> Ptr XRenderPictFormat -> CULong -> Ptr XRenderPictureAttributes -> IO Picture

-- Attributes not supported
instance Storable XRenderPictureAttributes where
    sizeOf _ = #{size XRenderPictureAttributes}
    alignment _ = alignment (undefined :: CInt)
    peek _ = return XRenderPictureAttributes
    poke p XRenderPictureAttributes =
        memset p 0 #{size XRenderPictureAttributes}

-- | Convenience function, gives us an XRender handle to a traditional
-- Pixmap.  Don't let it escape.
withRenderPicture :: Display -> Drawable -> (Picture -> IO a) -> IO ()
withRenderPicture d p f = do
    format <- xRenderFindStandardFormat d 1 -- PictStandardRGB24
    alloca $ \attr -> do
        pic <- xRenderCreatePicture d p format 0 attr
        f pic
        xRenderFreePicture d pic

-- | Convenience function, gives us an XRender picture that is a solid
-- fill of color 'c'.  Don't let it escape.
withRenderFill :: Display -> XRenderColor -> (Picture -> IO a) -> IO ()
withRenderFill d c f = do
    pic <- with c (xRenderCreateSolidFill d)
    f pic
    xRenderFreePicture d pic

-- | Drawing the background to a pixmap and taking into account
-- transparency
drawBackground ::  Display -> Drawable -> String -> Int -> Rectangle -> IO ()
drawBackground d p bgc alpha (Rectangle x y wid ht) = do
  let render opt bg pic m =
        xRenderComposite d opt bg m pic
                        (fromIntegral x) (fromIntegral y) 0 0
                        0 0 (fromIntegral wid) (fromIntegral ht)
  withRenderPicture d p $ \pic -> do
    -- Handle background color
    bgcolor <- parseRenderColor d bgc
    withRenderFill d bgcolor $ \bgfill ->
      withRenderFill d
                     (XRenderColor 0 0 0 (257 * alpha))
                     (render pictOpSrc bgfill pic)
    -- Handle transparency
    internAtom d "_XROOTPMAP_ID" False >>= \xid ->
      let xroot = defaultRootWindow d in
      alloca $ \x1 ->
      alloca $ \x2 ->
      alloca $ \x3 ->
      alloca $ \x4 ->
      alloca $ \pprop -> do
        xGetWindowProperty d xroot xid 0 1 False 20 x1 x2 x3 x4 pprop
        prop <- peek pprop
        when (prop /= nullPtr) $ do
          rootbg <- peek (castPtr prop) :: IO Pixmap
          xFree prop
          withRenderPicture d rootbg $ \bgpic ->
            withRenderFill d (XRenderColor 0 0 0 (0xFFFF - 257 * alpha))
                           (render pictOpAdd bgpic pic)

-- | Parses color into XRender color (allocation not necessary!)
parseRenderColor :: Display -> String -> IO XRenderColor
parseRenderColor d c = do
    let colormap = defaultColormap d (defaultScreen d)
    Color _ red green blue _ <- parseColor d colormap c
    return $ XRenderColor (fromIntegral red)
                          (fromIntegral green)
                          (fromIntegral blue)
                          0xFFFF

pictOpSrc, pictOpAdd :: PictOp
pictOpSrc = 1
pictOpAdd = 12

-- pictOpMinimum = 0
-- pictOpClear = 0
-- pictOpDst = 2
-- pictOpOver = 3
-- pictOpOverReverse = 4
-- pictOpIn = 5
-- pictOpInReverse = 6
-- pictOpOut = 7
-- pictOpOutReverse = 8
-- pictOpAtop = 9
-- pictOpAtopReverse = 10
-- pictOpXor = 11
-- pictOpSaturate = 13
-- pictOpMaximum = 13
