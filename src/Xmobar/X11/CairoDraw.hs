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
import Xmobar.X11.CairoSurface

type ActionPos = ([Action], Position, Position)
type Actions = [ActionPos]
type LayoutInfo = (Segment, P.PangoLayout, Double, Double)

drawInPixmap :: Pixmap -> Dimension -> Dimension -> [[Segment]] -> X Actions
drawInPixmap p w h s = do
  xconf <- ask
  let disp = display xconf
      vis = defaultVisualOfScreen (defaultScreenOfDisplay disp)
      c = config xconf
      fi = fromIntegral
      render = renderSegments c w h s
  liftIO $ withXlibSurface disp p vis (fi w) (fi h) render

segmentMarkup :: Config -> Segment -> String
segmentMarkup conf (Text txt, info, idx, _actions) =
  let fnt = fixXft $ indexedFont conf idx
      (fg, bg) = colorComponents conf (tColorsString info)
      attrs = [P.FontDescr fnt, P.FontForeground fg]
      attrs' = if bg == bgColor conf then attrs else P.FontBackground bg:attrs
  in P.markSpan attrs' $ P.escapeMarkup txt
segmentMarkup _ _ = ""

withLayoutInfo :: P.PangoContext -> Double -> Config -> Segment -> IO LayoutInfo
withLayoutInfo ctx maxh conf seg@(Text _, inf, idx, a) = do
  lyt <- P.layoutEmpty ctx
  mk <- P.layoutSetMarkup lyt (segmentMarkup conf seg) :: IO String
  (_, P.PangoRectangle o u w h) <- P.layoutGetExtents lyt
  let voff' = fromIntegral $ indexedOffset conf idx
      voff = voff' + (maxh - h + u) / 2.0
  return ((Text mk, inf, idx, a), lyt, w - o, voff)

withLayoutInfo ctx _ _ seg = do
  lyt <- P.layoutEmpty ctx
  let n = case seg of (Hspace w, _, _, _) -> w; _ -> 0
  return (seg, lyt, fromIntegral n, 0)

renderLayout :: Surface -> Double -> (Double, Actions)
             -> LayoutInfo -> IO (Double, Actions)
renderLayout surface maxoff (off, actions) (segment, lyt, lwidth, voff) =
  if off + lwidth > maxoff
  then pure (off, actions)
  else do
    C.renderWith surface $ C.moveTo off voff >> P.showLayout lyt
    let end = round $ off + lwidth
        (_, _, _, a) = segment
        actions' = case a of Just as -> (as, round off, end):actions; _ -> actions
    return (off + lwidth, actions')

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

layoutsWidth :: [(Segment, P.PangoLayout, Double, Double)] -> Double
layoutsWidth = foldl (\a (_,_,w,_) -> a + w) 0

renderSegments ::
  Config -> Dimension -> Dimension -> [[Segment]] -> Surface -> IO Actions
renderSegments conf w h segments surface = do
  let [left, center, right] = take 3 segments
      dh = fromIntegral h
      dw = fromIntegral w
  ctx <- P.cairoCreateContext Nothing
  llyts <- mapM (withLayoutInfo ctx dh conf) left
  rlyts <- mapM (withLayoutInfo ctx dh conf) right
  clyts <- mapM (withLayoutInfo ctx dh conf) center
  renderBackground conf surface
  (lend, as) <- foldM (renderLayout surface dw) (0, []) llyts
  let rw = layoutsWidth rlyts
      rstart = max (lend + 1) (dw - rw - 1)
      cmax = rstart - 1
      cw = layoutsWidth clyts
      cstart = lend + 1 + max 0 (dw - rw - lend - cw) / 2.0
  (_, as') <- foldM (renderLayout surface cmax) (cstart, as) clyts
  (_, as'') <- foldM (renderLayout surface dw) (rstart, as') rlyts
  when (borderWidth conf > 0) (renderBorder conf dw dh surface)
  return as''
