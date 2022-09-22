{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.X11.Text
-- Copyright   :  (C) 2011-2015, 2017, 2018, 2022 Jose Antonio Ortega Ruiz
--                (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  jao@gnu.org
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Xmobar.X11.Text
    ( XFont
    , initFont
    , textExtents
    , textWidth
    ) where

import qualified Control.Exception as E
import qualified Foreign as F
import qualified System.Mem.Weak as W

import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as Xx

type XFont = Xx.FontSet

initFont :: X.Display -> String -> IO XFont
initFont = initUtf8Font

miscFixedFont :: String
miscFixedFont = "-misc-fixed-*-*-*-*-*-*-*-*-*-*-*-*"

-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initUtf8Font :: X.Display -> String -> IO Xx.FontSet
initUtf8Font d s = do
  (_,_,f) <- E.handle fallBack getIt
  W.addFinalizer f (Xx.freeFontSet d f)
  return f
      where getIt = Xx.createFontSet d s
            fallBack :: E.SomeException -> IO ([String], String, Xx.FontSet)
            fallBack = const $ Xx.createFontSet d miscFixedFont

textWidth :: X.Display -> XFont -> String -> IO Int
textWidth _   fs s = return $ fromIntegral $ Xx.wcTextEscapement fs s

textExtents :: XFont -> String -> IO (F.Int32, F.Int32)
textExtents fs s = do
  let (_,rl)  = Xx.wcTextExtents fs s
      ascent  = fromIntegral $ negate (X.rect_y rl)
      descent = fromIntegral $ X.rect_height rl + fromIntegral (X.rect_y rl)
  return (ascent, descent)
