{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.X11.XlibDraw
-- Copyright: (c) 2018, 2020, 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sat Nov 24, 2018 18:49
--
--
-- Drawing the xmobar contents using Xlib and Xft primitives
--
------------------------------------------------------------------------------


module Xmobar.X11.XlibDraw (drawInPixmap, updateActions) where

import Prelude hiding (lookup)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map hiding ((\\), foldr, map, filter)
import Data.List ((\\))
import Data.Maybe (fromJust, isJust)
import qualified Data.List.NonEmpty as NE

import Graphics.X11.Xlib hiding (textExtents, textWidth, Segment)
import Graphics.X11.Xlib.Extras

import Xmobar.Config.Types
import Xmobar.Run.Parsers hiding (parseString)
import Xmobar.Run.Actions
import qualified Xmobar.X11.Bitmap as B
import Xmobar.X11.Types
import Xmobar.X11.Text
import Xmobar.X11.ColorCache
import Xmobar.System.Utils (safeIndex)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

drawInPixmap :: GC -> Pixmap -> Dimension -> Dimension -> [[Segment]] -> X ()
drawInPixmap gc p wid ht ~[left,center,right] = do
  r <- ask
  let c = config r
      d = display r
      fs = fontList r
      vs = verticalOffsets r
      strLn = liftIO . mapM getWidth
      iconW i = maybe 0 B.width (lookup i $ iconCache r)
      getWidth (Text s,cl,i,_) =
        textWidth d (safeIndex fs i) s >>= \tw -> return (Text s,cl,i,fi tw)
      getWidth (Icon s,cl,i,_) = return (Icon s,cl,i,fi $ iconW s)
      getWidth (Hspace s,cl,i,_) = return (Hspace s,cl,i,fi s)

  withColors d [bgColor c, borderColor c] $ \[bgcolor, bdcolor] -> do
    liftIO $ setForeground d gc bgcolor
    liftIO $ fillRectangle d p gc 0 0 wid ht

    -- write to the pixmap the new string
    printStrings p gc fs vs 1 L [] =<< strLn left
    printStrings p gc fs vs 1 R [] =<< strLn right
    printStrings p gc fs vs 1 C [] =<< strLn center
    -- draw border if requested
    liftIO $ drawBorder (border c) (borderWidth c) d p gc bdcolor wid ht

verticalOffset :: (Integral b, Integral a, MonadIO m) =>
                  a -> Widget -> XFont -> Int -> Config -> m b
verticalOffset ht (Text t) fontst voffs _
  | voffs > -1 = return $ fi voffs
  | otherwise = do
     (as,ds) <- liftIO $ textExtents fontst t
     let margin = (fi ht - fi ds - fi as) `div` 2
     return $ fi as + margin - 1
verticalOffset ht (Icon _) _ _ conf
  | iconOffset conf > -1 = return $ fi (iconOffset conf)
  | otherwise = return $ fi (ht `div` 2) - 1
verticalOffset _ (Hspace _) _ voffs _ = return $ fi voffs

printString :: Display -> Drawable -> XFont -> GC
            -> String -> String
            -> Position -> Position -> Position -> Position
            -> String -> Int
            -> IO ()

printString d p fs gc fc bc x y _ _ s a =
   withColors d [fc, bc] $ \[fc', bc'] -> do
     setForeground d gc fc'
     when (a == 255) (setBackground d gc bc')
     liftIO $ wcDrawImageString d p fs gc x y s

-- | An easy way to print the stuff we need to print
printStrings :: Drawable
             -> GC
             -> NE.NonEmpty XFont
             -> NE.NonEmpty Int
             -> Position
             -> Align
             -> [((Position, Position), Box)]
             -> [(Widget, TextRenderInfo, Int, Position)] -> X ()
printStrings _ _ _ _ _ _ _ [] = return ()
printStrings dr gc fontlist voffs offs a boxes sl@((s,c,i,l):xs) = do
  r <- ask
  let conf = config r
      d = display r
      alph = alpha conf
      Rectangle _ _ wid ht = rect r
      totSLen = foldr (\(_,_,_,len) -> (+) len) 0 sl
      remWidth = fi wid - fi totSLen
      fontst = safeIndex fontlist i
      voff = safeIndex voffs i
      offset = case a of
                 C -> (remWidth + offs) `div` 2
                 R -> remWidth
                 L -> offs
      (fc,bc) = colorComponents conf (tColorsString c)
  valign <- verticalOffset ht s fontst voff conf
  let (ht',ay) = case (tBgTopOffset c, tBgBottomOffset c) of
                   (-1,_)  -> (0, -1)
                   (_,-1)  -> (0, -1)
                   (ot,ob) -> (fromIntegral ht - ot - ob, ob)
  case s of
    (Text t) -> liftIO $ printString d dr fontst gc fc bc offset valign ay ht' t alph
    (Icon p) -> liftIO $ maybe (return ())
                           (B.drawBitmap d dr gc fc bc offset valign)
                           (lookup p (iconCache r))
    (Hspace _) -> liftIO $ return ()
  let triBoxes = tBoxes c
      dropBoxes = filter (\(_,b) -> b `notElem` triBoxes) boxes
      boxes' = map (\((x1,_),b) -> ((x1, offset + l), b))
                   (filter (\(_,b) -> b `elem` triBoxes) boxes)
            ++ map ((offset, offset + l),) (triBoxes \\ map snd boxes)
  if Prelude.null xs
    then liftIO $ drawBoxes d dr gc (fromIntegral ht) (dropBoxes ++ boxes')
    else liftIO $ drawBoxes d dr gc (fromIntegral ht) dropBoxes
  printStrings dr gc fontlist voffs (offs + l) a boxes' xs

drawBoxes :: Display
          -> Drawable
          -> GC
          -> Position
          -> [((Position, Position), Box)]
          -> IO ()
drawBoxes _ _ _ _ [] = return ()
drawBoxes d dr gc ht (b:bs) = do
  let (xx, Box bb offset lineWidth fc mgs) = b
      lw = fromIntegral lineWidth :: Position
  withColors d [fc] $ \[fc'] -> do
    setForeground d gc fc'
    setLineAttributes d gc lineWidth lineSolid capNotLast joinMiter
    case bb of
      BBVBoth -> do
        drawBoxBorder d dr gc BBTop    offset ht xx lw mgs
        drawBoxBorder d dr gc BBBottom offset ht xx lw mgs
      BBHBoth -> do
        drawBoxBorder d dr gc BBLeft   offset ht xx lw mgs
        drawBoxBorder d dr gc BBRight  offset ht xx lw mgs
      BBFull  -> do
        drawBoxBorder d dr gc BBTop    offset ht xx lw mgs
        drawBoxBorder d dr gc BBBottom offset ht xx lw mgs
        drawBoxBorder d dr gc BBLeft   offset ht xx lw mgs
        drawBoxBorder d dr gc BBRight  offset ht xx lw mgs
      _ -> drawBoxBorder d dr gc bb    offset ht xx lw mgs
  drawBoxes d dr gc ht bs

drawBorder :: Border -> Int -> Display -> Drawable -> GC -> Pixel
              -> Dimension -> Dimension -> IO ()
drawBorder b lw d p gc c wi ht =  case b of
  NoBorder -> return ()
  TopB       -> drawBorder (TopBM 0) lw d p gc c wi ht
  BottomB    -> drawBorder (BottomBM 0) lw d p gc c wi ht
  FullB      -> drawBorder (FullBM 0) lw d p gc c wi ht
  TopBM m    -> sf >> sla >>
                 drawLine d p gc 0 (fi m + boff) (fi wi) (fi m + boff)
  BottomBM m -> let rw = fi ht - fi m + boff in
                 sf >> sla >> drawLine d p gc 0 rw (fi wi) rw
  FullBM m   -> let mp = fi m
                    pad = 2 * fi mp +  fi lw
                in sf >> sla >>
                     drawRectangle d p gc mp mp (wi - pad) (ht - pad)
  where sf    = setForeground d gc c
        sla   = setLineAttributes d gc (fi lw) lineSolid capNotLast joinMiter
        boff  = borderOffset b lw

borderOffset :: (Integral a) => Border -> Int -> a
borderOffset b lw =
  case b of
    BottomB    -> negate boffs
    BottomBM _ -> negate boffs
    TopB       -> boffs
    TopBM _    -> boffs
    _          -> 0
  where boffs = calcBorderOffset lw

calcBorderOffset :: (Integral a) => Int -> a
calcBorderOffset = ceiling . (/2) . toDouble
  where toDouble = fi :: (Integral a) => a -> Double


drawBoxBorder :: Display
              -> Drawable
              -> GC
              -> BoxBorder
              -> BoxOffset
              -> Position
              -> (Position, Position)
              -> Position
              -> BoxMargins
              -> IO ()
drawBoxBorder
  d dr gc pos (BoxOffset alg offset) ht (x1,x2) lw (BoxMargins mt mr mb ml) = do
  let (p1,p2) = case alg of
                 L -> (0,      -offset)
                 C -> (offset, -offset)
                 R -> (offset, 0      )
      lc = lw `div` 2
  case pos of
    BBTop    -> drawLine d dr gc (x1 + p1) (mt + lc) (x2 + p2) (mt + lc)
    BBBottom -> do
      let lc' = max lc 1 + mb
      drawLine d dr gc (x1 + p1) (ht - lc') (x2 + p2) (ht - lc')
    BBLeft   -> drawLine d dr gc (x1 - 1 + ml) p1 (x1 - 1 + ml) (ht + p2)
    BBRight  -> drawLine d dr gc (x2 + lc - 1 - mr) p1 (x2 + lc - 1 - mr) (ht + p2)
    _ -> error "unreachable code"


updateActions :: Rectangle -> [[Segment]] -> X [([Action], Position, Position)]
updateActions (Rectangle _ _ wid _) ~[left,center,right] = do
  conf <- ask
  let d = display conf
      fs = fontList conf
      strLn :: [Segment] -> IO [(Maybe [Action], Position, Position)]
      strLn  = liftIO . mapM getCoords
      iconW i = maybe 0 B.width (lookup i $ iconCache conf)
      getCoords (Text s,_,i,a) =
        textWidth d (safeIndex fs i) s >>= \tw -> return (a, 0, fi tw)
      getCoords (Icon s,_,_,a) = return (a, 0, fi $ iconW s)
      getCoords (Hspace w,_,_,a) = return (a, 0, fi w)
      partCoord off xs = map (\(a, x, x') -> (fromJust a, x, x')) $
                         filter (\(a, _,_) -> isJust a) $
                         scanl (\(_,_,x') (a,_,w') -> (a, x', x' + w'))
                               (Nothing, 0, off)
                               xs
      totSLen = foldr (\(_,_,len) -> (+) len) 0
      remWidth xs = fi wid - totSLen xs
      offs = 1
      offset a xs = case a of
                     C -> (remWidth xs + offs) `div` 2
                     R -> remWidth xs
                     L -> offs
  liftIO $ fmap concat $ mapM (\(a,xs) ->
                          (\xs' -> partCoord (offset a xs') xs') <$> strLn xs) $
                         zip [L,C,R] [left,center,right]
