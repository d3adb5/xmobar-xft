------------------------------------------------------------------------------
-- |
-- Module: Xmobar.X11.Boxes
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Start date: Fri Sep 16, 2022 04:01
--
-- Borders and boxes
--
------------------------------------------------------------------------------

module Xmobar.Draw.Boxes (Line, boxLines, BoxRect, borderRect) where

import qualified Xmobar.Config.Types as T
import qualified Xmobar.Run.Parsers as P

type Line = (Double, Double, Double, Double)
type BoxRect = (Double, Double, Double, Double)

-- | Computes the coordinates of a list of lines representing a Box.
-- The Box is to be positioned between x0 and x1, with height ht, and drawn
-- with line width lw.  The returned lists are coordinates of the beginning
-- and end of each line.
boxLines :: P.Box -> Double -> Double -> Double -> [Line]
boxLines (P.Box bd offset lw _ margins) ht x0 x1 =
  case bd of
    P.BBTop    -> [rtop]
    P.BBBottom -> [rbot]
    P.BBVBoth  -> [rtop, rbot]
    P.BBLeft   -> [rleft]
    P.BBRight  -> [rright]
    P.BBHBoth  -> [rleft, rright]
    P.BBFull   -> [rtop, rbot, rleft, rright]
  where
    (P.BoxMargins top right bot left) = margins
    (P.BoxOffset align m) = offset
    ma = fromIntegral m
    (p0, p1) = case align of
                 T.L -> (0, -ma)
                 T.C -> (ma, -ma)
                 T.R -> (ma, 0)
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

-- | Computes the rectangle (x, y, width, height) for the given Border.
borderRect :: T.Border -> Double -> Double -> BoxRect
borderRect bdr w h =
  case bdr of
    T.TopB       -> (0, 0, w - 1, 0)
    T.BottomB    -> (0, h - 1, w - 1, 0)
    T.FullB      -> (0, 0, w - 1, h - 1)
    T.TopBM m    -> (0, fi m, w - 1, 0)
    T.BottomBM m -> (0, h - fi m, w - 1, 0)
    T.FullBM m   -> (fi m, fi m, w - 2 * fi m, h - 2 * fi m)
    T.NoBorder   -> (-1, -1, -1, -1)
  where fi = fromIntegral
