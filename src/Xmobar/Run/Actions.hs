-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Run.Actions
-- Copyright   :  (c) Alexander Polakov
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Xmobar.Run.Actions ( Button
                          , Action(..)
                          , runAction
                          , runAction'
                          , stripActions) where

import System.Process (spawnCommand, waitForProcess)
import Control.Monad (void)
import Text.Regex (Regex, subRegex, mkRegex, matchRegex)
import Data.Word (Word32)

type Button = Word32

data Action = Spawn [Button] String deriving (Eq, Read, Show)

runAction :: Action -> IO ()
runAction (Spawn _ s) = void $ spawnCommand (s ++ " &") >>= waitForProcess

-- | Run action with stdout redirected to stderr
runAction' :: Action -> IO ()
runAction' (Spawn _ s) = void $ spawnCommand (s ++ " 1>&2 &") >>= waitForProcess

stripActions :: String -> String
stripActions s = case matchRegex actionRegex s of
  Nothing -> s
  Just _  -> stripActions strippedOneLevel
  where
      strippedOneLevel = subRegex actionRegex s "[action=\\1\\2]\\3[/action]"

actionRegex :: Regex
actionRegex = mkRegex "<action=`?([^>`]*)`?( +button=[12345]+)?>(.+)</action>"
