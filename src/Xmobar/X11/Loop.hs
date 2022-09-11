{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.X11EventLoop
-- Copyright: (c) 2018, 2020, 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sat Nov 24, 2018 19:40
--
--
-- Event loop
--
------------------------------------------------------------------------------

module Xmobar.X11.Loop (x11Loop) where

import Prelude hiding (lookup)
import Graphics.X11.Xlib hiding (textExtents, textWidth, Segment)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama
import Graphics.X11.Xrandr

import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM

import Data.Bits
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Xmobar.System.Signal
import Xmobar.Config.Types ( persistent
                           , alpha
                           , font
                           , additionalFonts
                           , textOffset
                           , textOffsets
                           , position
                           , iconRoot
                           , Config
                           , XPosition(..))

import Xmobar.Run.Actions
import Xmobar.Run.Parsers
import Xmobar.X11.Window
import Xmobar.X11.Text
import Xmobar.X11.Draw
import Xmobar.X11.Bitmap as Bitmap
import Xmobar.X11.Types
import Xmobar.System.Utils (forkThread)

import Xmobar.Run.Loop (loop)

#ifndef THREADED_RUNTIME
import Xmobar.X11.Events(nextEvent')
#endif

#ifdef XFT
import Graphics.X11.Xft
#endif

runX :: XConf -> X a -> IO a
runX xc f = runReaderT f xc

-- | Starts the main event loop and threads
x11Loop :: Config -> IO ()
x11Loop conf = do
  initThreads
  d <- openDisplay ""
  fs <- initFont d (font conf)
  fl <- mapM (initFont d) (additionalFonts conf)
  let ic = Map.empty
      to = textOffset conf
      ts = textOffsets conf ++ replicate (length fl) to
#ifdef XFT
  xftInitFtLibrary
#endif
  (r,w) <- createWin d fs conf
  loop conf (startLoop (XConf d r w (fs :| fl) (to :| ts) ic conf))

startLoop :: XConf -> TMVar SignalType -> TVar [String] -> IO ()
startLoop xcfg@(XConf _ _ w _ _ _ _) sig tv = do
    forkThread "X event handler" (x11EventLoop w sig)
    signalLoop xcfg [] sig tv

-- | Translates X11 events received by w to signals handled by signalLoop
x11EventLoop :: Window -> TMVar SignalType -> IO ()
x11EventLoop w signal =
  allocaXEvent $ \e -> do
    dpy <- openDisplay ""
    xrrSelectInput dpy (defaultRootWindow dpy) rrScreenChangeNotifyMask
    selectInput dpy w (exposureMask .|. structureNotifyMask .|. buttonPressMask)

    forever $ do
#ifdef THREADED_RUNTIME
      nextEvent dpy e
#else
      nextEvent' dpy e
#endif
      ev <- getEvent e
      case ev of
        ConfigureEvent {} -> atomically $ putTMVar signal Reposition
        ExposeEvent {} -> atomically $ putTMVar signal Wakeup
        RRScreenChangeNotifyEvent {} -> atomically $ putTMVar signal Reposition
        ButtonEvent {} -> atomically $
               putTMVar signal (Action (ev_button ev) (fi $ ev_x ev))
        _ -> return ()

-- | Continuously wait for a signal from a thread or an interrupt handler
-- The list of actions provide also the positions of clickable rectangles
signalLoop :: XConf
          -> [([Action], Position, Position)]
          -> TMVar SignalType
          -> TVar [String]
          -> IO ()
signalLoop xc@(XConf d r w fs vos is cfg) as signal tv = do
      typ <- atomically $ takeTMVar signal
      case typ of
         Wakeup -> do
            segs <- updateSegments cfg tv
            xc' <- updateCache d w is (iconRoot cfg) segs >>=
                     \c -> return xc { iconS = c }
            as' <- runX xc' $ drawInWin segs
            signalLoop xc' as' signal tv

         Reposition ->
            reposWindow cfg

         ChangeScreen -> do
            ncfg <- updateConfigPosition cfg
            reposWindow ncfg

         Hide   t -> hide   (t*100*1000)
         Reveal t -> reveal (t*100*1000)
         Toggle t -> toggle t

         TogglePersistent -> signalLoop
            xc { config = cfg { persistent = not $ persistent cfg } } as signal tv

         SetAlpha a -> signalLoop xc { config = cfg { alpha = a}} as signal tv

         Action but x -> action but x

    where
        isPersistent = not $ persistent cfg
        loopOn = signalLoop xc as signal tv
        hide t
            | t == 0 =
                when isPersistent (hideWindow d w) >> loopOn
            | otherwise = do
                void $ forkIO
                     $ threadDelay t >> atomically (putTMVar signal $ Hide 0)
                loopOn

        reveal t
            | t == 0 = do
                when isPersistent (showWindow r cfg d w)
                loopOn
            | otherwise = do
                void $ forkIO
                     $ threadDelay t >> atomically (putTMVar signal $ Reveal 0)
                loopOn

        toggle t = do
            ismapped <- isMapped d w
            atomically (putTMVar signal $ if ismapped then Hide t else Reveal t)
            loopOn

        reposWindow rcfg = do
          r' <- repositionWin d w (NE.head fs) rcfg
          signalLoop (XConf d r' w fs vos is rcfg) as signal tv

        updateConfigPosition ocfg =
          case position ocfg of
            OnScreen n o -> do
              srs <- getScreenInfo d
              return (if n == length srs
                       then
                        (ocfg {position = OnScreen 1 o})
                       else
                        (ocfg {position = OnScreen (n+1) o}))
            o -> return (ocfg {position = OnScreen 1 o})

        action button x = do
          mapM_ runAction $
            filter (\(Spawn b _) -> button `elem` b) $
            concatMap (\(a,_,_) -> a) $
            filter (\(_, from, to) -> x >= from && x <= to) as
          loopOn

updateSegments :: Config -> TVar [String] -> IO [[Segment]]
updateSegments conf v = do
  s <- readTVarIO v
  let l:c:r:_ = s ++ repeat ""
  liftIO $ mapM (parseString conf) [l, c, r]
