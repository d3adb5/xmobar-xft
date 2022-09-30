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

import Xmobar.Config.Types (Config, Segment)
import Xmobar.Run.Actions (Action)

type Position = Double
type ActionPos = ([Action], Position, Position)
type Actions = [ActionPos]

type IconLookup = String -> (Double, Double)
type IconDrawer = Double -> Double -> String -> IO ()

data DrawContext = DC { dcIconDrawer :: IconDrawer
                      , dcIconLookup :: IconLookup
                      , dcConfig :: Config
                      , dcWidth :: Double
                      , dcHeight :: Double
                      , dcSegments :: [[Segment]]
                      }
