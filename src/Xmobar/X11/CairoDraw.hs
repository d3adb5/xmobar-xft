{-# LANGUAGE CPP #-}
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

import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.Names as CNames

import Control.Monad.IO.Class
import Control.Monad.Reader

import Graphics.X11.Xlib hiding (Segment, drawSegments)
import Graphics.Rendering.Cairo.Types
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Pango as P

import Xmobar.Run.Parsers (Segment
                          , Widget(..)
                          , Box (..)
                          , BoxMargins (..)
                          , BoxBorder (..)
                          , BoxOffset (..)
                          , TextRenderInfo (..)
                          , colorComponents)
import Xmobar.Config.Types
import Xmobar.Text.Pango (fixXft)
import Xmobar.X11.Types
import qualified Xmobar.X11.Bitmap as B
#ifdef XRENDER
import Xmobar.X11.XRender (drawBackground)
#endif
import Xmobar.X11.CairoSurface

type Renderinfo = (Segment, Surface -> Double -> Double -> IO (), Double)
type BitmapDrawer = Double -> Double -> String -> IO ()
type Actions = [ActionPos]

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
      conf = config xconf
      dc = DC (drawXBitmap xconf gc p) (lookupXBitmap xconf) conf dw dh s
      render = drawSegments dc
#ifdef XRENDER
  liftIO $ drawBackground disp p (bgColor conf) (alpha conf) (Rectangle 0 0 w h)
#endif
  liftIO $ withXlibSurface disp p vis (fromIntegral w) (fromIntegral h) render

drawXBitmap :: XConf -> GC -> Pixmap -> BitmapDrawer
drawXBitmap xconf gc p h v path = do
  let disp = display xconf
      conf = config xconf
      fc = fgColor conf
      bc = bgColor conf
      bm = lookupXBitmap xconf path
  liftIO $ maybe (return ()) (B.drawBitmap disp p gc fc bc (round h) (round v)) bm

lookupXBitmap :: XConf -> String -> Maybe B.Bitmap
lookupXBitmap xconf path = lookup path (iconCache xconf)

readColourName :: String -> (SRGB.Colour Double, Double)
readColourName str =
  case CNames.readColourName str of
    Just c -> (c, 1.0)
    Nothing -> case SRGB.sRGB24reads str of
                 [(c, "")] -> (c, 1.0)
                 [(c,d)] -> (c, read ("0x" ++ d))
                 _ ->  (CNames.white, 1.0)

setSourceColor :: (SRGB.Colour Double, Double) -> C.Render ()
setSourceColor (colour, alph) =
  if alph < 1 then C.setSourceRGBA r g b alph else C.setSourceRGB r g b
  where rgb = SRGB.toSRGB colour
        r = SRGB.channelRed rgb
        g = SRGB.channelGreen rgb
        b = SRGB.channelBlue rgb

renderRect :: String -> Double -> (Double, Double, Double, Double) -> C.Render ()
renderRect color wd (x0, y0, w, h) = do
  setSourceColor (readColourName color)
  C.setLineWidth wd
  C.rectangle x0 y0 w h
  C.stroke

renderLines :: String -> Double -> [(Double, Double, Double, Double)] -> C.Render ()
renderLines color wd lns = do
  setSourceColor (readColourName color)
  C.setLineWidth wd
  mapM_ (\(x0, y0, x1, y1) -> C.moveTo x0 y0 >> C.lineTo x1 y1 >> C.stroke) lns

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
      wd = w - o
      slyt s off mx = do
        when (off + w > mx) $ do
          P.layoutSetEllipsize lyt P.EllipsizeEnd
          P.layoutSetWidth lyt (Just $ mx - off)
        C.renderWith s $ C.moveTo off voff >> P.showLayout lyt
  return ((Text mk, inf, idx, a), slyt, wd)

withRenderinfo _ _ seg@(Hspace w, _, _, _) =
  return (seg, \_ _ _ -> return (), fromIntegral w)

withRenderinfo _ dctx seg@(Icon p, _, _, _) = do
  let bm = dcBitmapLookup dctx p
      wd = maybe 0 (fromIntegral . B.width) bm
      ioff = iconOffset (dcConfig dctx)
      vpos = dcHeight dctx / 2  + fromIntegral ioff
      draw _ off mx = when (off + wd <= mx) $ dcBitmapDrawer dctx off vpos p
  return (seg, draw, wd)

boxLines :: Box -> Double -> Double -> Double -> [(Double, Double, Double, Double)]
boxLines (Box bd offset lw _ margins) ht x0 x1 =
  case bd of
    BBTop  -> [rtop];   BBBottom -> [rbot];   BBVBoth -> [rtop, rbot]
    BBLeft -> [rleft];  BBRight  -> [rright]; BBHBoth -> [rleft, rright]
    BBFull -> [rtop, rbot, rleft, rright]
  where (BoxMargins top right bot left) = margins
        (BoxOffset align m) = offset
        ma = fromIntegral m
        (p0, p1) = case align of L -> (0, -ma); C -> (ma, -ma); R -> (ma, 0)
        lc = fromIntegral lw / 2
        [mt, mr, mb, ml] = map fromIntegral [top, right, bot, left]
        xmin = x0 - ml - lc
        xmax = x1 + mr + lc
        ymin = mt + lc
        ymax = ht - mb - lc
        rtop = (xmin + p0, ymin, xmax + p1, ymin)
        rbot = (xmin + p0, ymax, xmax + p1, ymax)
        rleft = (xmin, ymin + p0, xmin, ymax + p1)
        rright = (xmax, ymin + p0, xmax, ymax + p1)

drawBox :: DrawContext -> Surface -> Double -> Double -> Box -> IO ()
drawBox dctx surf x0 x1 box@(Box _ _ w color _) =
  C.renderWith surf $
    renderLines color (fromIntegral w) (boxLines box (dcHeight dctx) x0 x1)

drawSegmentBackground ::
  DrawContext -> Surface -> TextRenderInfo -> Double -> Double -> IO ()
drawSegmentBackground dctx surf info x0 x1 =
  when (bg /= bgColor conf && (top >= 0 || bot >= 0)) $
    C.renderWith surf $ do
      setSourceColor (readColourName bg)
      C.rectangle x0 top (x1 - x0) (dcHeight dctx - bot - top)
      C.fillPreserve
  where conf = dcConfig dctx
        (_, bg) = colorComponents conf (tColorsString info)
        top = fromIntegral $ tBgTopOffset info
        bot = fromIntegral $ tBgBottomOffset info

type Boxes = [(Double, Double, [Box])]
type SegAcc = (Double, Actions, Boxes)

drawSegment :: DrawContext -> Surface -> Double -> SegAcc -> Renderinfo -> IO SegAcc
drawSegment dctx surface maxoff (off, acts, boxs) (segment, render, lwidth) = do
  let end = min maxoff (off + lwidth)
      (_, info, _, a) = segment
      acts' = case a of Just as -> (as, round off, round end):acts; _ -> acts
      bs = tBoxes info
      boxs' = if null bs then boxs else (off, end, bs):boxs
  drawSegmentBackground dctx surface info off end
  render surface off maxoff
  return (off + lwidth, acts', boxs')

renderOuterBorder :: Config -> Double -> Double -> C.Render ()
renderOuterBorder conf w h =  do
  let r = case border conf of
            TopB -> (0, 0, w - 1, 0)
            BottomB -> (0, h - 1, w - 1, h - 1)
            FullB -> (0, 0, w - 1, h - 1)
            TopBM m -> (0, fi m, w - 1, fi m)
            BottomBM m -> (0, h - fi m, w - 1, h - fi m)
            FullBM m -> (fi m, fi m, w - fi m - 1, h - fi m - 1)
            NoBorder -> (-1, -1, -1, -1)
  renderRect (borderColor conf) (fi (borderWidth conf)) r
  where fi = fromIntegral

drawBorder :: Config -> Double -> Double -> Surface -> IO ()
drawBorder conf w h surf =
  case border conf of
    NoBorder -> return ()
    _ -> C.renderWith surf (renderOuterBorder conf w h)

layoutsWidth :: [Renderinfo] -> Double
layoutsWidth = foldl (\a (_,_,w) -> a + w) 0

drawBoxes' :: DrawContext -> Surface -> (Double, Double, [Box]) -> IO ()
drawBoxes' dctx surf (from, to, bs) = mapM_ (drawBox dctx surf from to) bs

drawBoxes :: DrawContext -> Surface -> Boxes -> IO ()
drawBoxes dctx surf ((from, to, b):(from', to', b'):bxs) =
  if to < from' || b /= b'
  then do drawBoxes' dctx surf (from, to, b)
          drawBoxes dctx surf $ (from', to', b'):bxs
  else drawBoxes dctx surf $ (from, to', b'):bxs

drawBoxes dctx surf [bi] = drawBoxes' dctx surf bi

drawBoxes _ _ [] = return ()

#ifndef XRENDER
drawCairoBackground :: DrawContext -> Surface -> IO ()
drawCairoBackground dctx surf = do
  let (c, _) = readColourName (bgColor (dcConfig dctx))
  C.renderWith surf $ setSourceColor (c, 1.0) >> C.paint
#endif

drawSegments :: DrawContext -> Surface -> IO Actions
drawSegments dctx surf = do
  let [left, center, right] = take 3 $ dcSegments dctx
      dh = dcHeight dctx
      dw = dcWidth dctx
      conf = dcConfig dctx
  ctx <- P.cairoCreateContext Nothing
  llyts <- mapM (withRenderinfo ctx dctx) left
  rlyts <- mapM (withRenderinfo ctx dctx) right
  clyts <- mapM (withRenderinfo ctx dctx) center
#ifndef XRENDER
  drawCairoBackground dctx surf
#endif
  (lend, as, bx) <- foldM (drawSegment dctx surf dw) (0, [], []) llyts
  let rw = layoutsWidth rlyts
      rstart = max (lend + 1) (dw - rw - 1)
      cmax = rstart - 1
      cw = layoutsWidth clyts
      cstart = lend + 1 + max 0 (dw - rw - lend - cw) / 2.0
  (_, as', bx') <- foldM (drawSegment dctx surf cmax) (cstart, as, bx) clyts
  (_, as'', bx'') <- foldM (drawSegment dctx surf dw) (rstart, as', bx') rlyts
  drawBoxes dctx surf (reverse bx'')
  when (borderWidth conf > 0) (drawBorder conf dw dh surf)
  return as''
