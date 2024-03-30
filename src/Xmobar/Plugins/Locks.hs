{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Locks
-- Copyright   :  (c) Patrick Chilton
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Patrick Chilton <chpatrick@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin that displays the status of the lock keys.
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Locks(Locks(..)) where

import Graphics.X11
import Data.List
import Data.List.Extra (trim)
import Data.Bits
import Data.Maybe  (fromJust)
import Control.Monad
import Control.Monad.Extra (ifM)
import Graphics.X11.Xlib.Extras
import Xmobar.Run.Exec
import Xmobar.System.Kbd
import Xmobar.X11.Events (nextEvent')

data Locks = Locks | Locks' [(String, (String, String))]
    deriving (Read, Show)

locks :: [ ( KeySym, String )]
locks = [ ( xK_Caps_Lock,   "CAPS" )
        , ( xK_Num_Lock,    "NUM" )
        , ( xK_Scroll_Lock, "SCROLL" )
        ]

type Labels = [ ( String, (String, String) )]
defaultLabels :: Labels
defaultLabels = let nms = map snd locks
                in zip nms (map (, mempty) nms)

type LabelledLock = (KeySym, String, String, String)

attach :: (KeySym, String) -> Labels -> LabelledLock
(key, lock) `attach` lbls = let (enb, dis) = fromJust $ lookup lock lbls
                            in (key, lock, enb, dis)

enabled :: (a, b, c, d) -> c
enabled (_, _, c, _) = c
disabled :: (a, b, c, d) -> d
disabled (_, _, _, d) = d

isEnabled :: (Bits a1, Foldable t, Foldable t1, Integral a)
  => Display -> t (a, t1 KeyCode) -> a1 -> (KeySym, b, c, d) -> IO Bool
isEnabled d modMap m ( ks, _, _, _ ) = do
    kc <- keysymToKeycode d ks
    return $ case find (elem kc . snd) modMap of
        Nothing       -> False
        Just ( i, _ ) -> testBit m (fromIntegral i)

run' :: Display -> Window -> Labels -> IO String
run' d root labels = do
    modMap <- getModifierMapping d
    ( _, _, _, _, _, _, _, m ) <- queryPointer d root

    ls' <- forM (map (`attach` labels) locks)
                (\l -> ifM (isEnabled d modMap m l)
                           (return (enabled l))
                           (return (disabled l)))
    return $ trim $ unwords ls'

instance Exec Locks where
    alias _ = "locks"
    start Locks cb = start (Locks' defaultLabels) cb
    start (Locks' labels) cb = do
        d <- openDisplay ""
        root <- rootWindow d (defaultScreen d)
        _ <- xkbSelectEventDetails d xkbUseCoreKbd xkbIndicatorStateNotify m m

        allocaXEvent $ \ep -> forever $ do
            cb =<< run' d root labels
            nextEvent' d ep
            getEvent ep

        closeDisplay d
        return ()
      where
        m = xkbAllStateComponentsMask
