-- |
-- Module: Xmobar.Text.Output
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Fri Feb 4, 2022 01:10
--
--
-- Format strings emitted by Commands into output strings
--
------------------------------------------------------------------------------

module Xmobar.Text.Output (initLoop, format) where

import Xmobar.Config.Types ( Config (..)
                           , TextOutputFormat (..)
                           , Segment
                           , Widget (..)
                           , tColorsString)


import Xmobar.Config.Parse (colorComponents)
import Xmobar.Config.Template (parseString)

import Xmobar.Text.Ansi (withAnsiColor)
import Xmobar.Text.Pango (withPangoMarkup)
import Xmobar.Text.Swaybar (formatSwaybar, prepare)

initLoop :: Config -> IO ()
initLoop conf = case textOutputFormat conf of
  Swaybar -> prepare
  _ -> return ()

formatWithColor :: Config -> Segment -> String
formatWithColor conf (Text s, info, idx, _) =
  case textOutputFormat conf of
    Ansi -> withAnsiColor (fg, bg) s
    Pango -> withPangoMarkup fg bg fn s
    _ -> s
  where (fg, bg) = colorComponents conf (tColorsString info)
        fonts = additionalFonts conf
        fn = if idx < 1 || idx > length fonts
             then font conf
             else fonts !! (idx - 1)
formatWithColor conf (Hspace n, i, x, y) =
   formatWithColor conf (Text $ replicate (fromIntegral n) ' ', i, x, y)
formatWithColor _ _ = ""

format :: Config -> String -> IO String
format conf s = do
  segments <- parseString conf s
  case textOutputFormat conf of
    Swaybar -> return $ formatSwaybar conf segments
    _ -> return (concatMap (formatWithColor conf) segments)
