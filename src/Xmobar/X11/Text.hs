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

import Control.Exception (SomeException, handle)

import Foreign
import Graphics.X11.Xlib hiding (textExtents, textWidth)
import Graphics.X11.Xlib.Extras
import System.Mem.Weak ( addFinalizer )

type XFont = FontSet

initFont :: Display -> String -> IO XFont
initFont = initUtf8Font

miscFixedFont :: String
miscFixedFont = "-misc-fixed-*-*-*-*-*-*-*-*-*-*-*-*"

-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initUtf8Font :: Display -> String -> IO FontSet
initUtf8Font d s = do
  (_,_,f) <- handle fallBack getIt
  addFinalizer f (freeFontSet d f)
  return f
      where getIt = createFontSet d s
            fallBack :: SomeException -> IO ([String], String, FontSet)
            fallBack = const $ createFontSet d miscFixedFont

textWidth :: Display -> XFont -> String -> IO Int
textWidth _   fs s = return $ fromIntegral $ wcTextEscapement fs s

textExtents :: XFont -> String -> IO (Int32,Int32)
textExtents fs s = do
  let (_,rl)  = wcTextExtents fs s
      ascent  = fromIntegral $ negate (rect_y rl)
      descent = fromIntegral $ rect_height rl + fromIntegral (rect_y rl)
  return (ascent, descent)
