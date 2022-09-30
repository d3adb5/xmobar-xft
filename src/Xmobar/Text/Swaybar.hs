{-# LANGUAGE DeriveGeneric #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Text.Swaybar
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Fri Feb 4, 2022 03:58
--
--
-- Segment codification using swaybar-protocol JSON strings
--
------------------------------------------------------------------------------

module Xmobar.Text.Swaybar (prepare, formatSwaybar) where

import Data.Aeson

import Data.ByteString.Lazy.UTF8 (toString)

import GHC.Generics

import Xmobar.Config.Types ( Config (additionalFonts)
                           , Segment
                           , Widget(..)
                           , Box(..)
                           , BoxBorder(..)
                           , FontIndex
                           , tBoxes
                           , tColorsString)

import Xmobar.Config.Parse (colorComponents)

import Xmobar.Text.SwaybarClicks (startHandler)
import Xmobar.Text.Pango (withPangoFont)

data Preamble =
  Preamble {version :: !Int, click_events :: Bool} deriving (Eq,Show,Generic)

asString :: ToJSON a => a -> String
asString = toString . encode

preamble :: String
preamble = (asString $ Preamble { version = 1, click_events = True }) ++ "\x0A["

data Block =
  Block { full_text :: !String
        , name :: !String
        , color :: Maybe String
        , background :: Maybe String
        , separator :: !Bool
        , separator_block_width :: !Int
        , border :: Maybe String
        , border_top :: Maybe Int
        , border_bottom :: Maybe Int
        , border_left :: Maybe Int
        , border_right :: Maybe Int
        , markup :: Maybe String
        } deriving (Eq,Show,Generic)


defaultBlock :: Block
defaultBlock = Block { full_text = ""
                     , name = ""
                     , color = Nothing
                     , background = Nothing
                     , separator = False
                     , separator_block_width = 0
                     , border = Nothing
                     , border_top = Nothing
                     , border_bottom = Nothing
                     , border_left = Nothing
                     , border_right = Nothing
                     , markup = Nothing
                     }

instance ToJSON Block where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

instance ToJSON Preamble

withBox :: Box -> Block -> Block
withBox (Box b _ n c _) block =
  (case b of
     BBFull -> bl { border_right = w, border_left = w
                  , border_bottom = w, border_top = w  }
     BBTop -> bl { border_top = w }
     BBBottom -> bl { border_bottom = w }
     BBVBoth -> bl { border_bottom = w, border_top = w }
     BBLeft -> bl { border_left = w }
     BBRight -> bl { border_right = w }
     BBHBoth -> bl { border_right = w, border_left = w }
  ) { border = bc }
  where w = Just (fromIntegral n)
        bc = if null c then Nothing else Just c
        j0 = Just 0
        bl = block { border_right = j0, border_left = j0
                   , border_bottom = j0, border_top = j0  }

withFont :: Config -> FontIndex -> Block -> Block
withFont conf idx block =
  if idx < 1 || idx > length fonts then block
  else block { markup = Just $ fonts !! (idx - 1) }
  where fonts = additionalFonts conf

withPango :: Block -> Block
withPango block = case markup block of
  Nothing -> block
  Just fnt -> block { full_text = txt fnt, markup = Just "pango"}
  where txt fn = withPangoFont fn (full_text block)

formatSwaybar' :: Config -> Segment -> Block
formatSwaybar' conf (Text txt, info, idx, as) =
  foldr withBox (withFont conf idx block) (tBoxes info)
  where (fg, bg) = colorComponents conf (tColorsString info)
        block = defaultBlock { full_text = txt
                             , color = Just fg
                             , background = Just bg
                             , name = show as
                             }
formatSwaybar' conf (Hspace n, info, i, a) =
  formatSwaybar' conf (Text (replicate (fromIntegral n) ' '), info, i, a)
formatSwaybar' _ _ = defaultBlock

collectBlock :: Block -> [Block] -> [Block]
collectBlock b [] = [b]
collectBlock b (h:bs) =
  if b {full_text = ""} == h {full_text = ""} then
    h {full_text = full_text b ++ full_text h} : bs
  else b:h:bs

collectSegment :: Config -> Segment -> [Block] -> [Block]
collectSegment config segment blocks =
  if null $ full_text b then blocks else collectBlock b blocks
  where b = formatSwaybar' config segment

formatSwaybar :: Config -> [Segment] -> String
formatSwaybar conf segs = asString (map withPango blocks) ++ ","
  where blocks = foldr (collectSegment conf) [] segs

prepare :: IO ()
prepare = startHandler >> putStrLn preamble
