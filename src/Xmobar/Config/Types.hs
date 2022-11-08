-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Config.Types
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The configuration types
--
-----------------------------------------------------------------------------

module Xmobar.Config.Types
    ( Config (..)
    , XPosition (..), Align (..), Border (..), TextOutputFormat (..)
    , Segment
    , FontIndex
    , Box(..)
    , BoxBorder(..)
    , BoxOffset(..)
    , BoxMargins(..)
    , TextRenderInfo(..)
    , Widget(..)
    , SignalChan (..)
    , Action (..)
    , Button
    ) where

import qualified Control.Concurrent.STM as STM
import qualified Xmobar.Run.Runnable as R
import qualified Xmobar.System.Signal as S

import Data.Int (Int32)
import Foreign.C.Types (CInt)

import Xmobar.Run.Actions (Action (..), Button)

-- $config
-- Configuration data type

-- | The configuration data type
data Config =
    Config { font :: String         -- ^ Font
           , additionalFonts :: [String] -- ^ List of alternative fonts
           , wmClass :: String      -- ^ X11 WM_CLASS property value
           , wmName :: String       -- ^ X11 WM_NAME property value
           , bgColor :: String      -- ^ Backgroud color
           , fgColor :: String      -- ^ Default font color
           , position :: XPosition  -- ^ Top Bottom or Static
           , textOutput :: Bool     -- ^ Write data to stdout instead of X
           , textOutputFormat :: TextOutputFormat
                -- ^ Which color format to use for stdout: Ansi or Pango
           , textOffset :: Int      -- ^ Offset from top of window for text
           , textOffsets :: [Int]   -- ^ List of offsets for additionalFonts
           , iconOffset :: Int      -- ^ Offset from top of window for icons
           , border :: Border       -- ^ NoBorder TopB BottomB or FullB
           , borderColor :: String  -- ^ Border color
           , borderWidth :: Int     -- ^ Border width
           , alpha :: Int           -- ^ Transparency from 0 (transparent)
                                    --   to 255 (opaque)
           , hideOnStart :: Bool    -- ^ Hide (Unmap) the window on
                                    --   initialization
           , allDesktops :: Bool    -- ^ Tell the WM to map to all desktops
           , overrideRedirect :: Bool -- ^ Needed for dock behaviour in some
                                      --   non-tiling WMs
           , pickBroadest :: Bool   -- ^ Use the broadest display
                                    --   instead of the first one by
                                    --   default
           , lowerOnStart :: Bool   -- ^ lower to the bottom of the
                                    --   window stack on initialization
           , persistent :: Bool     -- ^ Whether automatic hiding should
                                    --   be enabled or disabled
           , iconRoot :: FilePath   -- ^ Root folder for icons
           , commands :: [R.Runnable] -- ^ For setting the command,
                                      --   the command arguments
                                    --   and refresh rate for the programs
                                    --   to run (optional)
           , sepChar :: String      -- ^ The character to be used for indicating
                                    --   commands in the output template
                                    --   (default '%')
           , alignSep :: String     -- ^ Separators for left, center and
                                    --   right text alignment
           , template :: String     -- ^ The output template
           , verbose :: Bool        -- ^ Emit additional debug messages
           , signal :: SignalChan   -- ^ Channel to send signals to xmobar
           } deriving (Read, Show)

-- | The position datatype
data XPosition = Top            -- ^ Top of the screen, full width, auto height

               | TopH           -- ^ Top of the screen, full width with
                                --   specific height
                  Int           -- ^ Height (in pixels)

                 -- | Top of the screen, full width with
                 --   specific height and margins
               | TopHM
                  Int           -- ^ Height (in pixels)
                  Int           -- ^ Left margin (in pixels)
                  Int           -- ^ Right margin (in pixels)
                  Int           -- ^ Top margin (in pixels)
                  Int           -- ^ Bottom margin (in pixels)

                 -- | Top of the screen with specific width
                 --   (as screen percentage) and alignment
               | TopW
                  Align         -- ^ Alignement (L|C|R)
                  Int           -- ^ Width as screen percentage (0-100)

                 -- | Top of the screen with specific width
                 --   (as screen percentage), height and
                 --   alignment
               | TopSize
                  Align         -- ^ Alignement (L|C|R)
                  Int           -- ^ Width as screen percentage (0-100)
                  Int           -- ^ Height (in pixels)

                 -- | Top of the screen with specific left/right
                 --   margins
               | TopP
                  Int           -- ^ Left margin (in pixels)
                  Int           -- ^ Right margin (in pixels)

                 -- | Bottom of the screen, full width, auto height
               | Bottom

               | BottomH        -- ^ Bottom of the screen, full width, with
                                --   specific height
                  Int           -- ^ Height (in pixels)

                 -- | Bottom of the screen with specific height
                 --   and margins
               | BottomHM
                  Int           -- ^ Height (in pixels)
                  Int           -- ^ Left margin (in pixels)
                  Int           -- ^ Right margin (in pixels)
                  Int           -- ^ Top margin (in pixels)
                  Int           -- ^ Bottom margin (in pixels)

                 -- | Bottom of the screen with specific
                 --   left/right margins
               | BottomP
                  Int           -- ^ Left margin (in pixels)
                  Int           -- ^ Bottom margin (in pixels)

                 -- | Bottom of the screen with specific width
                 --   (as screen percentage) and alignment
                 --   and alignment
               | BottomW
                  Align         -- ^ Alignement (L|C|R)
                  Int           -- ^ Width as screen percentage (0-100)

                 -- | Bottom of the screen with specific width
                 --   (as screen percentage), height
                 --   and alignment
               | BottomSize
                  Align         -- ^ Alignement (L|C|R)
                  Int           -- ^ Width as screen percentage (0-100)
                  Int           -- ^ Height (in pixels)

                 -- | Static position and specific size
               | Static { xpos :: Int   -- ^ Position X (in pixels)
                        , ypos :: Int   -- ^ Position Y (in pixels)
                        , width :: Int  -- ^ Width (in pixels)
                        , height :: Int -- ^ Height (in pixels)
                        }

                 -- | Along with the position characteristics
                 --   specify the screen to display the bar
               | OnScreen
                  Int           -- ^ Screen id (primary is 0)
                  XPosition     -- ^ Position
                 deriving ( Read, Show, Eq )

data Align = L | R | C deriving ( Read, Show, Eq )

data Border = NoBorder
            | TopB
            | BottomB
            | FullB
            | TopBM Int
            | BottomBM Int
            | FullBM Int
              deriving ( Read, Show, Eq )

data TextOutputFormat = Plain | Ansi | Pango | Swaybar deriving (Read, Show, Eq)

type FontIndex = Int

newtype SignalChan = SignalChan {unSignalChan :: Maybe (STM.TMVar S.SignalType)}

instance Read SignalChan where
  readsPrec _ _ = fail "SignalChan is not readable from a String"

instance Show SignalChan where
  show (SignalChan (Just _)) = "SignalChan (Just <tmvar>)"
  show (SignalChan Nothing) = "SignalChan Nothing"

data Widget = Icon String | Text String | Hspace Int32 deriving Show

data BoxOffset = BoxOffset Align Int32 deriving (Eq, Show)

-- margins: Top, Right, Bottom, Left
data BoxMargins = BoxMargins Int32 Int32 Int32 Int32 deriving (Eq, Show)

data BoxBorder = BBTop
               | BBBottom
               | BBVBoth
               | BBLeft
               | BBRight
               | BBHBoth
               | BBFull
                 deriving (Read, Eq, Show)

data Box = Box { bBorder :: BoxBorder
               , bOffset :: BoxOffset
               , bWidth :: CInt
               , bColor :: String
               , bMargins :: BoxMargins
               } deriving (Eq, Show)

data TextRenderInfo = TextRenderInfo { tColorsString   :: String
                                     , tBgTopOffset    :: Int32
                                     , tBgBottomOffset :: Int32
                                     , tBoxes          :: [Box]
                                     } deriving Show

type Segment = (Widget, TextRenderInfo, FontIndex, Maybe [Action])
