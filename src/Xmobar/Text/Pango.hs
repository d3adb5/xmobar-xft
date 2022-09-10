------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Text.Pango
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Author: Pavel Kalugin
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Fri Feb 4, 2022 01:15
--
--
-- Codification with Pango markup
--
------------------------------------------------------------------------------

module Xmobar.Text.Pango (withPangoColor, withPangoFont, withPangoMarkup, fixXft)
where

import Text.Printf (printf)
import Data.List (isPrefixOf)

replaceAll :: (Eq a) => a -> [a] -> [a] -> [a]
replaceAll c s = concatMap (\x -> if x == c then s else [x])

xmlEscape :: String -> String
xmlEscape s = replaceAll '"' "&quot;" $
              replaceAll '\'' "&apos;" $
              replaceAll '<' "&lt;" $
              replaceAll '>' "&gt;" $
              replaceAll '&' "&amp;" s

withPangoColor :: (String, String) -> String -> String
withPangoColor (fg, bg) s =
  printf fmt (xmlEscape fg) (xmlEscape bg) (xmlEscape s)
  where fmt = "<span foreground=\"%s\" background=\"%s\">%s</span>"

fixXft :: String -> String
fixXft font =
  if "xft:" `isPrefixOf` font then replaceAll '-' " " $ drop 4 font else font

withPangoFont :: String -> String -> String
withPangoFont font txt = printf fmt (fixXft font) (xmlEscape txt)
  where fmt = "<span font=\"%s\">%s</span>"

withPangoMarkup :: String -> String -> String -> String -> String
withPangoMarkup fg bg font txt =
  printf fmt (fixXft font) (xmlEscape fg) (xmlEscape bg) (xmlEscape txt)
  where fmt = "<span font=\"%s\" foreground=\"%s\" background=\"%s\">%s</span>"
