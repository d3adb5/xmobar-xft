------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Types
-- Copyright: (c) 2018, 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sat Nov 24, 2018 19:02
--
--
-- The Xmobar basic type
--
------------------------------------------------------------------------------


module Xmobar.X11.Types where

import qualified Graphics.X11.Xlib as X11
import qualified Data.List.NonEmpty as NE

import Control.Monad.Reader (ReaderT)

import Xmobar.Config.Types
import Xmobar.Run.Actions (Action)
import Xmobar.Run.Parsers (Segment)
import Xmobar.X11.Bitmap (Bitmap, BitmapCache)
import Xmobar.X11.Text (XFont)

-- | The X type is a ReaderT
type X = ReaderT XConf IO

-- | The ReaderT inner component
data XConf =
    XConf { display   :: X11.Display
          , rect      :: X11.Rectangle
          , window    :: X11.Window
          , fontList  :: NE.NonEmpty XFont
          , iconCache :: BitmapCache
          , config    :: Config
          }

type ActionPos = ([Action], X11.Position, X11.Position)
type Actions = [ActionPos]

type BitmapDrawer = Double -> Double -> String -> IO ()

data DrawContext = DC { dcBitmapDrawer :: BitmapDrawer
                      , dcBitmapLookup :: String -> Maybe Bitmap
                      , dcConfig :: Config
                      , dcWidth :: Double
                      , dcHeight :: Double
                      , dcSegments :: [[Segment]]
                      }
