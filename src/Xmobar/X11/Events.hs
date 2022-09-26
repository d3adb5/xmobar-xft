------------------------------------------------------------------------------
-- |
-- Module: Xmobar.X11.Events
-- Copyright: (c) 2018, 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Nov 25, 2018 23:24
--
--
-- Utilities or event handling
--
------------------------------------------------------------------------------


module Xmobar.X11.Events(nextEvent') where

import qualified Control.Concurrent as C
import qualified System.Posix.Types as T

import qualified Graphics.X11.Xlib as X

-- | A version of nextEvent that does not block in foreign calls.
nextEvent' :: X.Display -> X.XEventPtr -> IO ()
nextEvent' d p = do
    pend <- X.pending d
    if pend /= 0
        then X.nextEvent d p
        else do
            C.threadWaitRead (T.Fd fd)
            nextEvent' d p
 where
    fd = X.connectionNumber d
