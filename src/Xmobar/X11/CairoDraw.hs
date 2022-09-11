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

import Prelude hiding (lookup)
import Data.Map (lookup)

import Control.Monad.IO.Class
import Control.Monad.Reader

import Graphics.X11.Xlib hiding (Segment)
import Graphics.Rendering.Cairo.Types
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Pango as P

import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.Names as CNames
import qualified Data.Colour.RGBSpace as RGBS

import Xmobar.Run.Parsers (Segment, Widget(..), colorComponents, tColorsString)
import Xmobar.Run.Actions (Action)
import Xmobar.Config.Types
import Xmobar.Text.Pango (fixXft)
import Xmobar.X11.Types
import qualified Xmobar.X11.Bitmap as B
import Xmobar.X11.CairoSurface

type ActionPos = ([Action], Position, Position)
type Actions = [ActionPos]
type Renderinfo = (Segment, Surface -> Double -> IO (), Double)
type BitmapDrawer = Double -> Double -> String -> IO ()

data DrawContext = DC { dcBitmapDrawer :: BitmapDrawer
                      , dcBitmapLookup :: String -> Maybe B.Bitmap
                      , dcConfig :: Config
                      , dcWidth :: Double
                      , dcHeight :: Double
                      , dcSegments :: [[Segment]]
                      }

drawInPixmap :: GC -> Pixmap -> [[Segment]] -> X Actions
drawInPixmap gc p s = do
  xconf <- ask
  let disp = display xconf
      vis = defaultVisualOfScreen (defaultScreenOfDisplay disp)
      (Rectangle _ _ w h) = rect xconf
      dw = fromIntegral w
      dh = fromIntegral h
      dc = DC (drawXBitmap xconf gc p) (lookupXBitmap xconf) (config xconf) dw dh s
      render = renderSegments dc
  liftIO $ withXlibSurface disp p vis (fromIntegral w) (fromIntegral h) render

lookupXBitmap :: XConf -> String -> Maybe B.Bitmap
lookupXBitmap xconf path = lookup path (iconCache xconf)

drawXBitmap :: XConf -> GC -> Pixmap -> BitmapDrawer
drawXBitmap xconf gc p h v path = do
  let disp = display xconf
      conf = config xconf
      fc = fgColor conf
      bc = bgColor conf
      bm = lookupXBitmap xconf path
  liftIO $ maybe (return ()) (B.drawBitmap disp p gc fc bc (round h) (round v)) bm

segmentMarkup :: Config -> Segment -> String
segmentMarkup conf (Text txt, info, idx, _actions) =
  let fnt = fixXft $ indexedFont conf idx
      (fg, bg) = colorComponents conf (tColorsString info)
      attrs = [P.FontDescr fnt, P.FontForeground fg]
      attrs' = if bg == bgColor conf then attrs else P.FontBackground bg:attrs
  in P.markSpan attrs' $ P.escapeMarkup txt
segmentMarkup _ _ = ""

withRenderinfo :: P.PangoContext -> DrawContext -> Segment -> IO Renderinfo
withRenderinfo ctx dctx seg@(Text _, inf, idx, a) = do
  let conf = dcConfig dctx
  lyt <- P.layoutEmpty ctx
  mk <- P.layoutSetMarkup lyt (segmentMarkup conf seg) :: IO String
  (_, P.PangoRectangle o u w h) <- P.layoutGetExtents lyt
  let voff' = fromIntegral $ indexedOffset conf idx
      voff = voff' + (dcHeight dctx - h + u) / 2.0
      slyt s pos = C.renderWith s $ C.moveTo pos voff >> P.showLayout lyt
  return ((Text mk, inf, idx, a), slyt, w - o)

withRenderinfo _ _ seg@(Hspace w, _, _, _) = do
  return (seg, \_ _ -> return (), fromIntegral w)

withRenderinfo _ dctx seg@(Icon p, _, idx, _) = do
  let bm = dcBitmapLookup dctx p
      wd = maybe 0 (fromIntegral . B.width) bm
      ioff = indexedOffset (dcConfig dctx) idx
      vpos = dcHeight dctx / 2  - fromIntegral ioff
      draw _ off = dcBitmapDrawer dctx off vpos p
  return (seg, draw, wd)

renderSegment ::
  Surface -> Double -> (Double, Actions) -> Renderinfo -> IO (Double, Actions)
renderSegment surface maxoff (off, acts) (segment, render, lwidth) =
  if off + lwidth > maxoff
  then pure (off, acts)
  else do
    render surface off
    let end = round $ off + lwidth
        (_, _, _, a) = segment
        acts' = case a of Just as -> (as, round off, end):acts; _ -> acts
    return (off + lwidth, acts')

setSourceColor :: RGBS.Colour Double -> C.Render ()
setSourceColor = RGBS.uncurryRGB C.setSourceRGB . SRGB.toSRGB

readColourName :: String -> IO (RGBS.Colour Double)
readColourName str =
  case CNames.readColourName str of
    Just c -> return c
    Nothing -> return $ SRGB.sRGB24read str

background :: Config -> SRGB.Colour Double -> C.Render ()
background conf colour = do
  setSourceColor colour
  C.paintWithAlpha $ fromIntegral (alpha conf) / 255.0

renderBackground :: Config -> Surface -> IO ()
renderBackground conf surface =
  when (alpha conf >= 255)
    (readColourName (bgColor conf) >>= C.renderWith surface . background conf)

drawRect :: String -> Double -> (Double, Double, Double, Double) -> C.Render()
drawRect name wd (x0, y0, x1, y1) = do
  col <- liftIO $ readColourName name
  setSourceColor col
  C.setLineWidth wd
  C.rectangle x0 y0 x1 y1
  C.strokePreserve

outerBorder :: Config -> Double -> Double -> C.Render ()
outerBorder conf w h =  do
  let r = case border conf of
            TopB -> (0, 0, w - 1, 0)
            BottomB -> (0, h - 1, w - 1, h - 1)
            FullB -> (0, 0, w - 1, h - 1)
            TopBM m -> (0, fi m, w - 1, fi m)
            BottomBM m -> (0, h - fi m, w - 1, h - fi m)
            FullBM m -> (fi m, fi m, w - fi m - 1, h - fi m - 1)
            NoBorder -> (-1, -1, -1, -1)
  drawRect (borderColor conf) (fi (borderWidth conf)) r
  where fi = fromIntegral

renderBorder :: Config -> Double -> Double -> Surface -> IO ()
renderBorder conf w h surf =
  case border conf of
    NoBorder -> return ()
    _ -> C.renderWith surf (outerBorder conf w h)

layoutsWidth :: [Renderinfo] -> Double
layoutsWidth = foldl (\a (_,_,w) -> a + w) 0

renderSegments :: DrawContext -> Surface -> IO Actions
renderSegments dctx surface = do
  let [left, center, right] = take 3 $ dcSegments dctx
      dh = dcHeight dctx
      dw = dcWidth dctx
      conf = dcConfig dctx
  ctx <- P.cairoCreateContext Nothing
  llyts <- mapM (withRenderinfo ctx dctx) left
  rlyts <- mapM (withRenderinfo ctx dctx) right
  clyts <- mapM (withRenderinfo ctx dctx) center
  renderBackground conf surface
  (lend, as) <- foldM (renderSegment surface dw) (0, []) llyts
  let rw = layoutsWidth rlyts
      rstart = max (lend + 1) (dw - rw - 1)
      cmax = rstart - 1
      cw = layoutsWidth clyts
      cstart = lend + 1 + max 0 (dw - rw - lend - cw) / 2.0
  (_, as') <- foldM (renderSegment surface cmax) (cstart, as) clyts
  (_, as'') <- foldM (renderSegment surface dw) (rstart, as') rlyts
  when (borderWidth conf > 0) (renderBorder conf dw dh surface)
  return as''
