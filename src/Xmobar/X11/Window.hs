-----------------------------------------------------------------------------
-- |
-- Module      :  Window
-- Copyright   :  (c) 2011-18, 2020-22 Jose A. Ortega Ruiz
--             :  (c) 2012 Jochen Keil
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Window manipulation functions
--
-----------------------------------------------------------------------------

module Xmobar.X11.Window where

import qualified Control.Monad as CM

import qualified Data.Function as DF
import qualified Data.List as DL
import qualified Data.Maybe as DM

import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as Xx

import qualified Graphics.X11.Xinerama as Xi
import qualified Foreign.C.Types as C

import qualified System.Posix.Process as PP

import qualified Xmobar.Config.Types as T
import qualified Xmobar.X11.Text as Txt

-- $window

-- | Creates a window with the attribute override_redirect set to True.
-- Windows Managers should not touch this kind of windows.
newWindow ::
  X.Display -> X.Screen -> X.Window -> X.Rectangle -> Bool -> IO X.Window
newWindow dpy scr rw (X.Rectangle x y w h) o = do
  let visual = X.defaultVisualOfScreen scr
      attrmask = if o then X.cWOverrideRedirect else 0
  X.allocaSetWindowAttributes $
         \attributes -> do
           X.set_override_redirect attributes o
           X.createWindow dpy rw x y w h 0 (X.defaultDepthOfScreen scr)
                        X.inputOutput visual attrmask attributes

-- | The function to create the initial window
createWin :: X.Display -> Txt.XFont -> T.Config -> IO (X.Rectangle, X.Window)
createWin d fs c = do
  let dflt = X.defaultScreen d
  srs <- Xi.getScreenInfo d
  rootw <- X.rootWindow d dflt
  (as,ds) <- Txt.textExtents fs "0"
  let ht = as + ds + 4
      r = setPosition c (T.position c) srs (fromIntegral ht)
  win <- newWindow  d (X.defaultScreenOfDisplay d) rootw r (T.overrideRedirect c)
  setProperties c d win
  setStruts r c d win srs
  CM.when (T.lowerOnStart c) $ X.lowerWindow d win
  CM.unless (T.hideOnStart c) $ showWindow r c d win
  return (r,win)

-- | Updates the size and position of the window
repositionWin :: X.Display -> X.Window -> Txt.XFont -> T.Config -> IO X.Rectangle
repositionWin d win fs c = do
  srs <- Xi.getScreenInfo d
  (as,ds) <- Txt.textExtents fs "0"
  let ht = as + ds + 4
      r = setPosition c (T.position c) srs (fromIntegral ht)
  X.moveResizeWindow d win
    (X.rect_x r) (X.rect_y r) (X.rect_width r) (X.rect_height r)
  setStruts r c d win srs
  X.sync d False
  return r

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

setPosition ::
  T.Config -> T.XPosition -> [X.Rectangle] -> X.Dimension -> X.Rectangle
setPosition c p rs ht =
  case p' of
    T.Top -> X.Rectangle rx ry rw h
    T.TopP l r -> X.Rectangle (rx + fi l) ry (rw - fi l - fi r) h
    T.TopH ch -> X.Rectangle rx ry rw (mh ch)
    T.TopHM ch l r t _ ->
      X.Rectangle (rx + fi l) (ry + fi t) (rw - fi l - fi r) (mh ch)
    T.TopW a i -> X.Rectangle (ax a i) ry (nw i) h
    T.TopSize a i ch -> X.Rectangle (ax a i) ry (nw i) (mh ch)
    T.Bottom -> X.Rectangle rx ny rw h
    T.BottomH ch -> X.Rectangle rx (ny' ch) rw (mh ch)
    T.BottomHM ch l r _ b ->
      X.Rectangle (rx + fi l) (ry + fi rh - fi b - fi (mh ch)) (rw - fi l - fi r) (mh ch)
    T.BottomW a i -> X.Rectangle (ax a i) ny (nw i) h
    T.BottomP l r -> X.Rectangle (rx + fi l) ny (rw - fi l - fi r) h
    T.BottomSize a i ch  -> X.Rectangle (ax a i) (ny' ch) (nw i) (mh ch)
    T.Static cx cy cw ch -> X.Rectangle (fi cx) (fi cy) (fi cw) (fi ch)
    T.OnScreen _ p'' -> setPosition c p'' [scr] ht
  where
    (scr@(X.Rectangle rx ry rw rh), p') =
      case p of T.OnScreen i x -> (DM.fromMaybe (picker rs) $ safeIndex i rs, x)
                _ -> (picker rs, p)
    ny = ry + fi (rh - ht)
    center i = rx + fi (div (remwid i) 2)
    right  i = rx + fi (remwid i)
    remwid i = rw - pw (fi i)
    ax T.L = const rx
    ax T.R = right
    ax T.C = center
    pw i = rw * min 100 i `div` 100
    nw = fi . pw . fi
    h = fi ht
    mh h' = max (fi h') h
    ny' h' = ry + fi (rh - mh h')
    safeIndex i = lookup i . zip [0..]
    picker = if T.pickBroadest c
             then DL.maximumBy (compare `DF.on` X.rect_width)
             else head

setProperties :: T.Config -> X.Display -> X.Window -> IO ()
setProperties c d w = do
  let mkatom n = X.internAtom d n False
  card <- mkatom "CARDINAL"
  atom <- mkatom "ATOM"

  X.setTextProperty d w (T.wmClass c) X.wM_CLASS
  X.setTextProperty d w (T.wmName c) X.wM_NAME

  wtype <- mkatom "_NET_WM_WINDOW_TYPE"
  dock <- mkatom "_NET_WM_WINDOW_TYPE_DOCK"
  Xx.changeProperty32 d w wtype atom Xx.propModeReplace [fi dock]

  CM.when (T.allDesktops c) $ do
    desktop <- mkatom "_NET_WM_DESKTOP"
    Xx.changeProperty32 d w desktop card Xx.propModeReplace [0xffffffff]

  pid  <- mkatom "_NET_WM_PID"
  PP.getProcessID >>=
    Xx.changeProperty32 d w pid card Xx.propModeReplace . return . fi

setStruts' :: X.Display -> X.Window -> [C.CLong] -> IO ()
setStruts' d w svs = do
  let mkatom n = X.internAtom d n False
  card <- mkatom "CARDINAL"
  pstrut <- mkatom "_NET_WM_STRUT_PARTIAL"
  strut <- mkatom "_NET_WM_STRUT"
  Xx.changeProperty32 d w pstrut card Xx.propModeReplace svs
  Xx.changeProperty32 d w strut card Xx.propModeReplace (take 4 svs)

setStruts ::
  X.Rectangle -> T.Config -> X.Display -> X.Window -> [X.Rectangle] -> IO ()
setStruts r c d w rs = do
  let svs = map fi $ getStrutValues r (T.position c) (getRootWindowHeight rs)
  setStruts' d w svs

getRootWindowHeight :: [X.Rectangle] -> Int
getRootWindowHeight srs = maximum (map getMaxScreenYCoord srs)
  where
    getMaxScreenYCoord sr = fi (X.rect_y sr) + fi (X.rect_height sr)

getStrutValues :: X.Rectangle -> T.XPosition -> Int -> [Int]
getStrutValues r@(X.Rectangle x y w h) p rwh =
  case p of
    T.OnScreen _ p'      -> getStrutValues r p' rwh
    T.Top                -> [0, 0, st  , 0   , 0, 0, 0, 0, nx, nw, 0 , 0 ]
    T.TopH    _          -> [0, 0, st  , 0   , 0, 0, 0, 0, nx, nw, 0 , 0 ]
    T.TopHM _ _ _ _ b    -> [0, 0, st+b, 0   , 0, 0, 0, 0, nx, nw, 0 , 0 ]
    T.TopP    _ _        -> [0, 0, st  , 0   , 0, 0, 0, 0, nx, nw, 0 , 0 ]
    T.TopW    _ _        -> [0, 0, st  , 0   , 0, 0, 0, 0, nx, nw, 0 , 0 ]
    T.TopSize      {}    -> [0, 0, st  , 0   , 0, 0, 0, 0, nx, nw, 0 , 0 ]
    T.Bottom             -> [0, 0, 0   , sb  , 0, 0, 0, 0, 0 , 0 , nx, nw]
    T.BottomH _          -> [0, 0, 0   , sb  , 0, 0, 0, 0, 0 , 0 , nx, nw]
    T.BottomHM _ _ _ t _ -> [0, 0, 0   , sb+t, 0, 0, 0, 0, 0 , 0 , nx, nw]
    T.BottomP _ _        -> [0, 0, 0   , sb  , 0, 0, 0, 0, 0 , 0 , nx, nw]
    T.BottomW _ _        -> [0, 0, 0   , sb  , 0, 0, 0, 0, 0 , 0 , nx, nw]
    T.BottomSize   {}    -> [0, 0, 0   , sb  , 0, 0, 0, 0, 0 , 0 , nx, nw]
    T.Static       {}    -> getStaticStrutValues p rwh
  where st = fi y + fi h
        sb = rwh - fi y
        nx = fi x
        nw = fi (x + fi w - 1)

-- get some reaonable strut values for static placement.
getStaticStrutValues :: T.XPosition -> Int -> [Int]
getStaticStrutValues (T.Static cx cy cw ch) rwh
    -- if the yPos is in the top half of the screen, then assume a Top
    -- placement, otherwise, it's a Bottom placement
    | cy < (rwh `div` 2) = [0, 0, st,  0, 0, 0, 0, 0, xs, xe,  0,  0]
    | otherwise = [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, xs, xe]
    where st = cy + ch
          sb = rwh - cy
          xs = cx -- a simple calculation for horizontal (x) placement
          xe = xs + cw - 1
getStaticStrutValues _ _ = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

hideWindow :: X.Display -> X.Window -> IO ()
hideWindow d w = do
    setStruts' d w (replicate 12 0)
    Xx.unmapWindow d w >> X.sync d False

showWindow :: X.Rectangle -> T.Config -> X.Display -> X.Window -> IO ()
showWindow r c d w = do
    X.mapWindow d w
    Xi.getScreenInfo d >>= setStruts r c d w
    X.sync d False

isMapped :: X.Display -> X.Window -> IO Bool
isMapped d w = ism <$> Xx.getWindowAttributes d w
    where ism Xx.WindowAttributes { Xx.wa_map_state = wms } = wms /= Xx.waIsUnmapped
