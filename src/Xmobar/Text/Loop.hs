------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Text.Loop
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Fri Jan 28, 2022 01:21
--
--
-- Text-only event loop
--
------------------------------------------------------------------------------

module Xmobar.Text.Loop (textLoop) where

import Prelude hiding (lookup)
import System.IO (hSetBuffering, stdin, stdout, BufferMode(LineBuffering))

import Control.Concurrent.STM

import Xmobar.System.Signal
import Xmobar.Config.Types (Config)
import Xmobar.Run.Loop (loop)
import Xmobar.Text.Output (initLoop, format)

-- | Starts the main event loop and threads
textLoop :: Config -> IO ()
textLoop conf = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  initLoop conf
  loop conf (eventLoop conf)

-- | Continuously wait for a signal from a thread or a interrupt handler
eventLoop :: Config -> TMVar SignalType -> TVar [String] -> IO ()
eventLoop cfg signal tv = do
  typ <- atomically $ takeTMVar signal
  case typ of
    Wakeup -> updateString cfg tv >>= putStrLn >> eventLoop cfg signal tv
    _ -> eventLoop cfg signal tv

updateString :: Config -> TVar [String] -> IO String
updateString conf v = do
  s <- readTVarIO v
  return $ format conf (concat s)
