{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.X11EventLoop
-- Copyright: (c) 2018, 2020, 2022, 2023, 2024 Jose Antonio Ortega Ruiz
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

import Control.Concurrent as Concurrent
import Control.Concurrent.STM as STM
import Control.Monad as MR
import Control.Monad.Reader as MR

import Data.Bits (Bits((.|.)))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map

import qualified Graphics.X11.Xlib as X11
import qualified Graphics.X11.Xlib.Extras as X11x
import qualified Graphics.X11.Xinerama as Xinerama
import qualified Graphics.X11.Xrandr as Xrandr

import qualified Xmobar.Config.Types as C
import qualified Xmobar.Config.Template as CT

import qualified Xmobar.Run.Actions as A
import qualified Xmobar.Run.Loop as L

import qualified Xmobar.System.Utils as U
import qualified Xmobar.System.Signal as S

import qualified Xmobar.Draw.Types as D

import qualified Xmobar.X11.Types as T
import qualified Xmobar.X11.Text as Text
import qualified Xmobar.X11.Draw as Draw
import qualified Xmobar.X11.Bitmap as Bitmap
import qualified Xmobar.X11.Window as W

#ifndef THREADED_RUNTIME
import qualified Xmobar.X11.Events as E
#endif

runX :: T.XConf -> T.X a -> IO a
runX xc f = MR.runReaderT f xc

-- | Starts the main event loop thread
x11Loop :: C.Config -> IO ()
x11Loop conf = do
  X11.initThreads
  d <- X11.openDisplay ""
  fs <- Text.initFont d (C.font conf)
  fl <- mapM (Text.initFont d) (C.additionalFonts conf)
  (r,w) <- W.createWin d fs conf
  L.loop conf (startLoop (T.XConf d r w (fs :| fl) Map.empty conf))

startLoop :: T.XConf -> STM.TMVar S.SignalType -> STM.TVar [String] -> IO ()
startLoop xcfg sig tv = do
  U.forkThread "X event handler" (eventLoop (T.display xcfg) (T.window xcfg) sig)
  signalLoop xcfg [] sig tv

-- | Translates X11 events received by w to signals handled by signalLoop
eventLoop :: X11.Display -> X11.Window -> STM.TMVar S.SignalType -> IO ()
eventLoop dpy w signalv =
  X11.allocaXEvent $ \e -> do
    let root = X11.defaultRootWindow dpy
        m = X11.exposureMask .|. X11.structureNotifyMask .|. X11.buttonPressMask
    Xrandr.xrrSelectInput dpy root X11.rrScreenChangeNotifyMask
    X11.selectInput dpy w m

    MR.forever $ do
#ifdef THREADED_RUNTIME
      X11.nextEvent dpy e
#else
      E.nextEvent' dpy e
#endif
      ev <- X11x.getEvent e
      let send = STM.atomically . STM.putTMVar signalv
      case ev of
        X11x.ConfigureEvent {}            -> send S.Reposition
        X11x.RRScreenChangeNotifyEvent {} -> send S.Reposition
        X11x.ExposeEvent {}               -> send S.Wakeup
        X11x.ButtonEvent {}               -> send (S.Action b p)
           where (b, p) = (X11x.ev_button ev, fromIntegral $ X11x.ev_x ev)
        _ -> return ()

-- | Continuously wait for a signal from a thread or an interrupt handler.
-- The list of actions provides the positions of clickable rectangles,
-- and there is a mutable variable for received signals and the list
-- of strings updated by running monitors.
signalLoop ::
  T.XConf -> D.Actions -> STM.TMVar S.SignalType -> STM.TVar [String] -> IO ()
signalLoop xc@(T.XConf d r w fs is cfg) actions signalv strs = do
    typ <- STM.atomically $ STM.takeTMVar signalv
    case typ of
      S.Wakeup           -> wakeup
      S.Action button x  -> runActions actions button x >> loopOn
      S.Reposition       -> reposWindow cfg
      S.ChangeScreen     -> updateConfigPosition d cfg >>= reposWindow
      S.Hide t           -> hiderev t S.Hide W.hideWindow
      S.Reveal t         -> hiderev t S.Reveal (W.showWindow r cfg)
      S.Toggle t         -> toggle t
      S.TogglePersistent -> updateCfg $ cfg {C.persistent = not $ C.persistent cfg}
      S.SetAlpha a       -> updateCfg $ cfg {C.alpha = a}
    where
        loopOn' xc' = signalLoop xc' actions signalv strs
        loopOn = loopOn' xc
        updateCfg cfg' = loopOn' (xc {T.config = cfg'})

        wakeup =  do
          segs <- parseSegments cfg strs
          xc' <- updateIconCache xc segs
          actions' <- runX xc' (Draw.draw segs)
          signalLoop xc' actions' signalv strs

        hiderev t sign op
            | t == 0 = MR.unless (C.persistent cfg) (op d w) >> loopOn
            | otherwise = do
                MR.void $ Concurrent.forkIO
                     $ Concurrent.threadDelay (t*100*1000) >>
                       STM.atomically (STM.putTMVar signalv $ sign 0)
                loopOn

        toggle t = do
          ismapped <- W.isMapped d w
          let s = if ismapped then S.Hide t else S.Reveal t
          STM.atomically (STM.putTMVar signalv s)
          loopOn

        reposWindow rcfg = do
          r' <- W.repositionWin d w (NE.head fs) rcfg
          signalLoop (T.XConf d r' w fs is rcfg) actions signalv strs

parseSegments :: C.Config -> STM.TVar [String] -> IO [[C.Segment]]
parseSegments conf v = do
  s <- STM.readTVarIO v
  return $ map (CT.parseString conf) (take 3 $ s ++ repeat "")

updateIconCache :: T.XConf -> [[C.Segment]] -> IO T.XConf
updateIconCache xc@(T.XConf d _ w _ c cfg) segs = do
  let paths = [p | (C.Icon p, _, _, _) <- concat segs]
  c' <- Bitmap.updateCache d w c (C.iconRoot cfg) paths
  return $ xc {T.iconCache = c'}

updateConfigPosition :: X11.Display -> C.Config -> IO C.Config
updateConfigPosition disp cfg =
  case C.position cfg of
    C.OnScreen n o -> do
      srs <- Xinerama.getScreenInfo disp
      return (if n == length srs
              then (cfg {C.position = C.OnScreen 1 o})
              else (cfg {C.position = C.OnScreen (n+1) o}))
    o -> return (cfg {C.position = C.OnScreen 1 o})

runActions :: D.Actions -> A.Button -> X11.Position -> IO ()
runActions actions button pos =
  mapM_ A.runAction $
   filter (\(A.Spawn b _) -> button `elem` b) $
   concatMap (\(a,_,_) -> a) $
   filter (\(_, from, to) -> pos' >= from && pos' <= to) actions
  where pos' = fromIntegral pos
