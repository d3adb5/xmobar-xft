------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Draw.Types
-- Copyright: (c) 2022 jao
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: mail@jao.io
-- Stability: unstable
-- Portability: portable
-- Created: Tue Sep 20, 2022 04:49
--
--
-- Type definitions for describing drawing operations
--
------------------------------------------------------------------------------


module Xmobar.Draw.Types where

import GHC.Word (Word32, Word64)

import Data.Map (Map)

import Xmobar.Config.Types (Config)
import Xmobar.Run.Actions (Action)
import Xmobar.Run.Parsers (Segment)

type Position = Double
type ActionPos = ([Action], Position, Position)
type Actions = [ActionPos]

type BitmapDrawer = Double -> Double -> String -> IO ()

data BitmapType = Mono Word64 | Poly

data Bitmap = Bitmap { bWidth  :: Word32
                     , bHeight :: Word32
                     , bPixmap :: Word64
                     , bShapepixmap :: Maybe Word64
                     , bBitmaptype :: BitmapType
                     }

type BitmapCache = Map FilePath Bitmap


data DrawContext = DC { dcBitmapDrawer :: BitmapDrawer
                      , dcBitmapLookup :: String -> Maybe Bitmap
                      , dcConfig :: Config
                      , dcWidth :: Double
                      , dcHeight :: Double
                      , dcSegments :: [[Segment]]
                      }
