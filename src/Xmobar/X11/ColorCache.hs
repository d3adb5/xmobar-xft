{-# LANGUAGE CPP #-}
------------------------------------------------------------------------------
-- |
-- Module: ColorCache
-- Copyright: (c) 2012, 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Mon Sep 10, 2012 00:27
--
--
-- Caching X colors
--
------------------------------------------------------------------------------

module Xmobar.X11.ColorCache(withColors) where

import qualified Data.IORef as IO
import qualified System.IO.Unsafe as U

import qualified Control.Exception as E
import qualified Control.Monad.Trans as Tr

import qualified Graphics.X11.Xlib as X

data DynPixel = DynPixel Bool X.Pixel

initColor :: X.Display -> String -> IO DynPixel
initColor dpy c = E.handle black $ initColor' dpy c
  where
    black :: E.SomeException -> IO DynPixel
    black = const . return $ DynPixel False (X.blackPixel dpy $ X.defaultScreen dpy)

type ColorCache = [(String, X.Color)]
{-# NOINLINE colorCache #-}
colorCache :: IO.IORef ColorCache
colorCache = U.unsafePerformIO $ IO.newIORef []

getCachedColor :: String -> IO (Maybe X.Color)
getCachedColor color_name = lookup color_name `fmap` IO.readIORef colorCache

putCachedColor :: String -> X.Color -> IO ()
putCachedColor name c_id = IO.modifyIORef colorCache $ \c -> (name, c_id) : c

initColor' :: X.Display -> String -> IO DynPixel
initColor' dpy c = do
  let colormap = X.defaultColormap dpy (X.defaultScreen dpy)
  cached_color <- getCachedColor c
  c' <- case cached_color of
          Just col -> return col
          _        -> do (c'', _) <- X.allocNamedColor dpy colormap c
                         putCachedColor c c''
                         return c''
  return $ DynPixel True (X.color_pixel c')

withColors :: Tr.MonadIO m => X.Display -> [String] -> ([X.Pixel] -> m a) -> m a
withColors d cs f = do
  ps <- mapM (Tr.liftIO . initColor d) cs
  f $ map (\(DynPixel _ pixel) -> pixel) ps
