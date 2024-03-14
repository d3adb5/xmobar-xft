-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Kbd
-- Copyright   :  (c) Martin Perner
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Martin Perner <martin@perner.cc>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A keyboard layout indicator for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Kbd(Kbd(..)) where

import Data.Bifunctor (bimap)
import Data.List (find, tails, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Char (toLower, isLetter)
import Data.Function ((&))
import Control.Monad (forever)
import Control.Applicative ((<|>))
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Xmobar.Run.Exec
import Xmobar.X11.Events (nextEvent')
import Xmobar.System.Kbd

-- some strong typing
newtype Lay = Lay String deriving (Eq)
newtype Sym = Sym String
instance Show Sym where show (Sym s) = s
type KbdOpts' = [(Lay, Sym)]
typed :: [(String, String)] -> [(Lay, Sym)]
typed = map (bimap Lay Sym)

-- 'Bad' prefixes of layouts
noLaySymbols :: [String]
noLaySymbols = ["group", "inet", "ctr", "compose", "pc", "ctrl", "terminate"]


-- splits the layout string into the actual layouts
splitLayout :: String -> [String]
splitLayout s
  = filter flt
  . map (takeWhile (/= ':'))
  $ split (=='+') s
  where
  flt "" = False
  flt s' = not $ any (`isPrefixOf` s') noLaySymbols

-- split String at each Char
split :: (Char -> Bool) -> String -> [String]
split p s = case break p s of
  (pref, _:suf) -> pref : split p suf
  (pref, "") -> [pref]

-- replaces input string if on search list (exact match) with corresponding
-- element on replacement list, and returns it paired with the following item
--
-- if not found, return string unchanged, paired with empty string
searchReplaceLayout :: KbdOpts' -> String -> (Lay, Lay, Sym)
searchReplaceLayout opts curr
  = maybe (Lay "", Lay "", Sym curr)
          pickSymWithAdjLays
          (find currLayout (tails $ cycle opts))
      where
        pickSymWithAdjLays ((l1, _):(_, s):(l2, _):_) = (l1, l2, s)
        pickSymWithAdjLays _ = error "This should never happen"
        currLayout = (Lay curr ==) . (!! 1) . map fst

-- returns the active layout and the following one
getCurAndNextKbdLays :: Display -> KbdOpts' -> IO (Lay, Lay, Sym)
getCurAndNextKbdLays dpy opts = do
  lay <- splitLayout <$> getLayoutStr dpy
  grps <- map (map toLower . take 2) <$> getGrpNames dpy
  curLay <- getKbdLayout dpy
  return $ searchReplaceLayout opts
         $ fromMaybe "??"
         $ (lay !!? curLay) <|> (grps !!? curLay)

(!!?) :: [a] -> Int -> Maybe a
(!!?) []       _ = Nothing
(!!?) (x : _)  0 = Just x
(!!?) (_ : xs) i = xs !!? (i - 1)

newtype Kbd = Kbd [(String, String)]
  deriving (Read, Show)

attachClickAction :: (Lay, Lay, Sym) -> Sym
attachClickAction (Lay prv, Lay nxt, txt) = txt & linkTo nxt `onKey` "1"
                                                & linkTo prv `onKey` "3"
  where
    splitLayParensPhon :: String -> (String, String, String)
    splitLayParensPhon = (\(a, (b, c)) -> (a, b, c))
                       . second (second (drop 1) . break (== ')') . drop 1)
                       . break (== '(')
    parseLayPhon :: String -> (Maybe String, Maybe String)
    parseLayPhon s = let (l, p, i) = splitLayParensPhon s
                         l' = if all isLetter l
                                then Just ("-layout " ++ l)
                                else Nothing
                         p' = if (p, i) == ("phonetic", "")
                                then Just "-variant phonetic"
                                else Nothing
                     in (l', p')
    linkTo :: String -> String -> Sym -> Sym
    linkTo linked button currLay = Sym $ case parseLayPhon linked of
          (Nothing, _) -> "??"
          (Just linkedLay, phon) -> wrapIn setxkbmap button currLay
            where
              setxkbmap = unwords ["setxkbmap", linkedLay, fromMaybe "" phon]
    wrapIn :: String -> String -> Sym -> String
    wrapIn action buttons (Sym sym) = openingTag ++ sym ++ closingTag
      where
        openingTag = "<action=`" ++ action ++ "` button=" ++ buttons ++ ">"
        closingTag = "</action>"
    onKey = ($)
    second = fmap

instance Exec Kbd where
        alias (Kbd _) = "kbd"
        start (Kbd opts) cb = do

            dpy <- openDisplay ""

            -- initial set of layout
            cb . show . attachClickAction =<< getCurAndNextKbdLays dpy (typed opts)

            -- enable listing for
            -- group changes
            _ <- xkbSelectEventDetails dpy xkbUseCoreKbd xkbStateNotify xkbAllStateComponentsMask xkbGroupStateMask
            -- layout/geometry changes
            _ <- xkbSelectEvents dpy  xkbUseCoreKbd xkbNewKeyboardNotifyMask xkbNewKeyboardNotifyMask

            allocaXEvent $ \e -> forever $ do
                nextEvent' dpy e
                _ <- getEvent e
                cb . show . attachClickAction =<< getCurAndNextKbdLays dpy (typed opts)

            closeDisplay dpy
