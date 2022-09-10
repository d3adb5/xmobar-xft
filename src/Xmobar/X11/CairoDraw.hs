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

drawInPixmap :: Pixmap -> Dimension -> Dimension -> [[Segment]] -> X Actions
drawInPixmap p w h s = do
  xconf <- ask
  let disp = display xconf
      vis = defaultVisualOfScreen (defaultScreenOfDisplay disp)
      c = config xconf
      fi = fromIntegral
      render = (renderSegments c w h s)
  liftIO $ withXlibSurface disp p vis (fi w) (fi h) render

withMarkup :: Config -> Segment -> String
withMarkup conf (Text txt, info, idx, _actions) =
  let fnt = fixXft $ indexedFont conf idx
      (fg, bg) = colorComponents conf (tColorsString info)
      attrs = [P.FontDescr fnt, P.FontForeground fg, P.FontBackground bg]
  in P.markSpan attrs $ P.escapeMarkup txt
withMarkup _ _ = ""

type FPair = (Position, Actions)

renderSegment ::
  Double -> Config -> Surface -> P.PangoLayout -> FPair -> Segment -> IO FPair
renderSegment mh conf surface lyt (offset,actions) seg@(Text _, _, idx, a) = do
  _ <- (P.layoutSetMarkup lyt (withMarkup conf seg)) :: IO String
  (_, P.PangoRectangle o u w h) <- P.layoutGetExtents lyt
  let voff' = fromIntegral $ indexedOffset conf idx
      voff = voff' + (mh - h + u) / 2.0
      hoff = fromIntegral offset
  C.renderWith surface $ C.moveTo hoff voff >> P.showLayout lyt
  let end = round $ hoff + o + w
      actions' = case a of Just as -> (as, offset, end):actions; _ -> actions
  return (end, actions')

renderSegment _ _ _ _ (offset,actions) (Hspace n, _, _, a) = do
  let end = offset + n
      actions' = case a of Just as -> (as, offset, end):actions; _ -> actions
  return (end, actions')

renderSegment _h _c _surface _lyt acc _segment = pure acc

background :: Config -> SRGB.Colour Double -> C.Render ()
background conf colour = do
  RGBS.uncurryRGB C.setSourceRGB (SRGB.toSRGB colour)
  C.paintWithAlpha $ (fromIntegral (alpha conf)) / 255.0

renderSegments ::
  Config -> Dimension -> Dimension -> [[Segment]] -> Surface -> IO Actions
renderSegments conf _w h segments surface = do
  ctx <- P.cairoCreateContext Nothing
  lyt <- P.layoutEmpty ctx
  col <- case CNames.readColourName (bgColor conf) of
           Just c -> return c
           Nothing -> return $ SRGB.sRGB24read (bgColor conf)
  C.renderWith surface (background conf col)
  let dh = fromIntegral h
  snd `fmap` foldM (renderSegment dh conf surface lyt) (0, []) (concat segments)
