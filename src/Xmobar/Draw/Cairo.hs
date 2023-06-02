{-# LANGUAGE CPP #-}
------------------------------------------------------------------------------
-- |
-- Module: Xmobar.X11.Cairo
-- Copyright: (c) 2022, 2023 Jose Antonio Ortega Ruiz
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

module Xmobar.Draw.Cairo (drawSegments) where

import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.Names as CNames

import Control.Monad (foldM, when)

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango

import Graphics.Rendering.Cairo.Types(Surface)

import qualified Xmobar.Config.Types as C
import qualified Xmobar.Config.Parse as ConfigParse
import qualified Xmobar.Text.Pango as TextPango

import qualified Xmobar.Draw.Boxes as Boxes
import qualified Xmobar.Draw.Types as T

type Renderinfo = (C.Segment, Surface -> Double -> Double -> IO (), Double)
type BoundedBox = (Double, Double, [C.Box])
type Acc = (Double, T.Actions, [BoundedBox])

readColourName :: String -> (SRGB.Colour Double, Double)
readColourName str =
  case CNames.readColourName str of
    Just c -> (c, 1.0)
    Nothing -> case SRGB.sRGB24reads str of
                 [(c, "")] -> (c, 1.0)
                 [(c,d)] -> (c, read ("0x" ++ d))
                 _ ->  (CNames.white, 1.0)

setSourceColor :: (SRGB.Colour Double, Double) -> Cairo.Render ()
setSourceColor (colour, alph) =
  if alph < 1 then Cairo.setSourceRGBA r g b alph else Cairo.setSourceRGB r g b
  where rgb = SRGB.toSRGB colour
        r = SRGB.channelRed rgb
        g = SRGB.channelGreen rgb
        b = SRGB.channelBlue rgb

renderLines :: String -> Double -> [Boxes.Line] -> Cairo.Render ()
renderLines color wd lns = do
  setSourceColor (readColourName color)
  Cairo.setLineWidth wd
  mapM_ (\(x0, y0, x1, y1) ->
           Cairo.moveTo x0 y0 >> Cairo.lineTo x1 y1 >> Cairo.stroke) lns

segmentMarkup :: C.Config -> C.Segment -> String
segmentMarkup conf (C.Text txt, info, idx, _actions) =
  let fnt = TextPango.fixXft $ ConfigParse.indexedFont conf idx
      (fg, bg) = ConfigParse.colorComponents conf (C.tColorsString info)
      attrs = [Pango.FontDescr fnt, Pango.FontForeground fg]
      attrs' = if bg == C.bgColor conf
               then attrs
               else Pango.FontBackground bg:attrs
  in Pango.markSpan attrs' $ Pango.escapeMarkup txt
segmentMarkup _ _ = ""

withRenderinfo :: Pango.PangoContext -> T.DrawContext -> C.Segment -> IO Renderinfo
withRenderinfo ctx dctx seg@(C.Text _, inf, idx, a) = do
  let conf = T.dcConfig dctx
  lyt <- Pango.layoutEmpty ctx
  mk <- Pango.layoutSetMarkup lyt (segmentMarkup conf seg) :: IO String
  (_, Pango.PangoRectangle o u w h) <- Pango.layoutGetExtents lyt
  let voff' = fromIntegral $ ConfigParse.indexedOffset conf idx
      voff = voff' + (T.dcHeight dctx - h + u) / 2.0
      wd = w - o
      slyt s off mx = do
        when (off + w > mx) $ do
          Pango.layoutSetEllipsize lyt Pango.EllipsizeEnd
          Pango.layoutSetWidth lyt (Just $ mx - off)
        Cairo.renderWith s $ Cairo.moveTo off voff >> Pango.showLayout lyt
  return ((C.Text mk, inf, idx, a), slyt, wd)

withRenderinfo _ _ seg@(C.Hspace w, _, _, _) =
  return (seg, \_ _ _ -> return (), fromIntegral w)

withRenderinfo _ dctx seg@(C.Icon p, info, _, _) = do
  let (wd, _) = T.dcIconLookup dctx p
      ioff = C.iconOffset (T.dcConfig dctx)
      vpos = T.dcHeight dctx / 2  + fromIntegral ioff
      conf = T.dcConfig dctx
      (fg, bg) = ConfigParse.colorComponents conf (C.tColorsString info)
      render _ off mx = when (off + wd <= mx) $
        T.dcIconDrawer dctx off vpos p fg bg
  return (seg, render, wd)

drawBox :: T.DrawContext -> Surface -> Double -> Double -> C.Box -> IO ()
drawBox dctx surf x0 x1 box@(C.Box _ _ w color _) =
  Cairo.renderWith surf $
    renderLines color (fromIntegral w) (Boxes.boxLines box (T.dcHeight dctx) x0 x1)

drawSegmentBackground ::
  T.DrawContext -> Surface -> C.TextRenderInfo -> Double -> Double -> IO ()
drawSegmentBackground dctx surf info x0 x1 =
  when (bg /= C.bgColor conf && (top >= 0 || bot >= 0)) $
    Cairo.renderWith surf $ do
      setSourceColor (readColourName bg)
      Cairo.rectangle x0 top (x1 - x0) (T.dcHeight dctx - bot - top)
      Cairo.fillPreserve
  where conf = T.dcConfig dctx
        (_, bg) = ConfigParse.colorComponents conf (C.tColorsString info)
        top = fromIntegral $ C.tBgTopOffset info
        bot = fromIntegral $ C.tBgBottomOffset info

drawSegment :: T.DrawContext -> Surface -> Double -> Acc -> Renderinfo -> IO Acc
drawSegment dctx surface maxoff (off, acts, boxs) (segment, render, lwidth) = do
  let end = min maxoff (off + lwidth)
      (_, info, _, a) = segment
      acts' = case a of Just as -> (as, off, end):acts; _ -> acts
      bs = C.tBoxes info
      boxs' = if null bs then boxs else (off, end, bs):boxs
  when (end > off) $ do
    drawSegmentBackground dctx surface info off end
    render surface off maxoff
  return (off + lwidth, acts', boxs')

renderOuterBorder :: C.Config -> Double -> Double -> Cairo.Render ()
renderOuterBorder conf mw mh = do
  let (x0, y0, w, h) = Boxes.borderRect (C.border conf) mw mh
  setSourceColor (readColourName (C.borderColor conf))
  Cairo.setLineWidth (fromIntegral (C.borderWidth conf))
  Cairo.rectangle x0 y0 w h
  Cairo.stroke

drawBorder :: C.Config -> Double -> Double -> Surface -> IO ()
drawBorder conf w h surf =
  case C.border conf of
    C.NoBorder -> return ()
    _ -> Cairo.renderWith surf (renderOuterBorder conf w h)

drawBBox :: T.DrawContext -> Surface -> BoundedBox -> IO ()
drawBBox dctx surf (from, to, bs) = mapM_ (drawBox dctx surf from to) bs

drawBoxes :: T.DrawContext -> Surface -> [BoundedBox] -> IO ()
drawBoxes dctx surf ((from, to, b):(from', to', b'):bxs) =
  if to < from' || b /= b'
  then do drawBBox dctx surf (from, to, b)
          drawBoxes dctx surf $ (from', to', b'):bxs
  else drawBoxes dctx surf $ (from, to', b'):bxs

drawBoxes dctx surf [bi] = drawBBox dctx surf bi

drawBoxes _ _ [] = return ()

#ifndef XRENDER
drawCairoBackground :: T.DrawContext -> Surface -> IO ()
drawCairoBackground dctx surf = do
  let (c, _) = readColourName (C.bgColor (T.dcConfig dctx))
  Cairo.renderWith surf $ setSourceColor (c, 1.0) >> Cairo.paint
#endif

drawSegments :: T.DrawContext -> Surface -> IO T.Actions
drawSegments dctx surf = do
  let [left, center, right] = take 3 $ T.dcSegments dctx ++ repeat []
      dh = T.dcHeight dctx
      dw = T.dcWidth dctx
      conf = T.dcConfig dctx
      sWidth = foldl (\a (_,_,w) -> a + w) 0
  ctx <- Pango.cairoCreateContext Nothing
  Pango.cairoContextSetResolution ctx $ C.dpi conf
  llyts <- mapM (withRenderinfo ctx dctx) left
  rlyts <- mapM (withRenderinfo ctx dctx) right
  clyts <- mapM (withRenderinfo ctx dctx) center
#ifndef XRENDER
  drawCairoBackground dctx surf
#endif
  (lend, as, bx) <- foldM (drawSegment dctx surf dw) (0, [], []) llyts
  let [rw, cw] = map sWidth [rlyts, clyts]
      rstart = max lend (dw - rw)
      cstart = if lend > 1 || rw == 0 then max lend ((dw - cw) / 2.0) else lend
  (_, as', bx') <- if cw > 0
                   then foldM (drawSegment dctx surf rstart) (cstart, as, bx) clyts
                   else return (0, as, bx)
  (_, as'', bx'') <- foldM (drawSegment dctx surf dw) (rstart, as', bx') rlyts
  drawBoxes dctx surf (reverse bx'')
  when (C.borderWidth conf > 0) (drawBorder conf dw dh surf)
  return as''
