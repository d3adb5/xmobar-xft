-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Disk
-- Copyright   :  (c) 2010-2012, 2014, 2018, 2019, 2024 Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
--  Disk usage and throughput monitors for Xmobar
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Xmobar.Plugins.Monitors.Disk (diskUConfig, runDiskU, startDiskIO) where

import Xmobar.Plugins.Monitors.Common
#if defined(freebsd_HOST_OS)
import qualified Xmobar.Plugins.Monitors.Disk.FreeBSD as MD
#else
import qualified Xmobar.Plugins.Monitors.Disk.Linux as MD
#endif

import Control.Monad (zipWithM)
import System.Console.GetOpt
import Data.List (find)

import Xmobar.Plugins.Monitors.Disk.Common (
  DevName
  , Path
  )

data DiskIOOpts = DiskIOOpts
  { totalIconPattern :: Maybe IconPattern
  , writeIconPattern :: Maybe IconPattern
  , readIconPattern :: Maybe IconPattern
  , contiguous :: Bool
  }

dioDefaultOpts :: DiskIOOpts
dioDefaultOpts = DiskIOOpts
   { totalIconPattern = Nothing
   , writeIconPattern = Nothing
   , readIconPattern = Nothing
   , contiguous = False
   }

dioOptions :: [OptDescr (DiskIOOpts -> DiskIOOpts)]
dioOptions =
   [ Option "" ["total-icon-pattern"] (ReqArg (\x o ->
      o { totalIconPattern = Just $ parseIconPattern x}) "") ""
   , Option "" ["write-icon-pattern"] (ReqArg (\x o ->
      o { writeIconPattern = Just $ parseIconPattern x}) "") ""
   , Option "" ["read-icon-pattern"] (ReqArg (\x o ->
      o { readIconPattern = Just $ parseIconPattern x}) "") ""
   , Option "c" ["contiguous"] (NoArg (\o -> o {contiguous = True})) ""
   ]

diskIOConfig :: IO MConfig
diskIOConfig = mkMConfig "" ["total", "read", "write"
                            ,"totalb", "readb", "writeb"
                            ,"totalbar", "readbar", "writebar"
                            ,"totalbbar", "readbbar", "writebbar"
                            ,"totalvbar", "readvbar", "writevbar"
                            ,"totalbvbar", "readbvbar", "writebvbar"
                            ,"totalipat", "readipat", "writeipat"
                            ,"totalbipat", "readbipat", "writebipat"
                            ]

data DiskUOpts = DiskUOpts
  { freeIconPattern :: Maybe IconPattern
  , usedIconPattern :: Maybe IconPattern
  , contiguousU :: Bool
  }

duDefaultOpts :: DiskUOpts
duDefaultOpts = DiskUOpts
   { freeIconPattern = Nothing
   , usedIconPattern = Nothing
   , contiguousU = False
   }

duOptions :: [OptDescr (DiskUOpts -> DiskUOpts)]
duOptions =
   [ Option "" ["free-icon-pattern"] (ReqArg (\x o ->
      o { freeIconPattern = Just $ parseIconPattern x}) "") ""
   , Option "" ["used-icon-pattern"] (ReqArg (\x o ->
      o { usedIconPattern = Just $ parseIconPattern x}) "") ""
   , Option "c" ["contiguous"] (NoArg (\o -> o {contiguousU = True})) ""
   ]

diskUConfig :: IO MConfig
diskUConfig = mkMConfig ""
              [ "size", "free", "used", "freep", "usedp"
              , "freebar", "freevbar", "freeipat"
              , "usedbar", "usedvbar", "usedipat"
              ]

speedToStr :: Float -> String
speedToStr = showWithUnits 2 1 . (/ 1024)

sizeToStr :: Integer -> String
sizeToStr = showWithUnits 3 0 . fromIntegral

runDiskIO' :: DiskIOOpts -> (String, [Float]) -> Monitor String
runDiskIO' opts (tmp, xs) = do
  s <- mapM (showWithColors speedToStr) xs
  b <- mapM (showLogBar 0.8) xs
  vb <- mapM (showLogVBar 0.8) xs
  ipat <- mapM (\(f,v) -> showLogIconPattern (f opts) 0.8 v)
        $ zip [totalIconPattern, readIconPattern, writeIconPattern
              , totalIconPattern, readIconPattern, writeIconPattern]
              xs
  setConfigValue tmp template
  parseTemplate $ s ++ b ++ vb ++ ipat

runDiskIO :: MD.DevDataRef -> [(String, String)] -> [String] -> Monitor String
runDiskIO dref disks argv = do
  opts <- io $ parseOptsWith dioOptions dioDefaultOpts argv
  stats <- io $ MD.fetchDataIO dref disks
  mounted <- io $ MD.fetchDataUsage disks
  strs <- mapM (runDiskIO' opts) $ devTemplates disks (map fst mounted) stats
  return $ (if contiguous opts then concat else unwords) strs

startDiskIO :: [(String, String)] ->
               [String] -> Int -> (String -> IO ()) -> IO ()
startDiskIO disks args rate cb = do
  dref <- MD.initializeDevDataRef disks
  runM args diskIOConfig (runDiskIO dref disks) rate cb

runDiskU' :: DiskUOpts -> String -> [Integer] -> Monitor String
runDiskU' opts tmp (total:free:diff:_) = do
  setConfigValue tmp template
  let strs = map sizeToStr [free, diff]
      freep = if total > 0 then free * 100 `div` total else 0
      fr = fromIntegral freep / 100
  s <- zipWithM showWithColors' strs [freep, 100 - freep]
  sp <- showPercentsWithColors [fr, 1 - fr]
  fb <- showPercentBar (fromIntegral freep) fr
  fvb <- showVerticalBar (fromIntegral freep) fr
  fipat <- showIconPattern (freeIconPattern opts) fr
  ub <- showPercentBar (fromIntegral $ 100 - freep) (1 - fr)
  uvb <- showVerticalBar (fromIntegral $ 100 - freep) (1 - fr)
  uipat <- showIconPattern (usedIconPattern opts) (1 - fr)
  parseTemplate $ [sizeToStr total] ++ s ++ sp ++ [fb,fvb,fipat,ub,uvb,uipat]
runDiskU' _ _ _ = return ""

runDiskU :: [(String, String)] -> [String] -> Monitor String
runDiskU disks argv = do
  opts <- io $ parseOptsWith duOptions duDefaultOpts argv
  stats <- io $ MD.fetchDataUsage disks
  strs <- mapM (\((d, p), stat) -> runDiskU' opts (findTempl d p disks) stat) stats
  return $ (if contiguousU opts then concat else unwords) strs

findTempl :: DevName -> Path -> [(String, String)] -> String
findTempl dev path disks =
  case find devOrPath disks of
    Just (_, t) -> t
    Nothing -> ""
  where devOrPath (d, _) = d == dev || d == path

devTemplates :: [(String, String)]
                -> [(DevName, Path)]
                -> [(DevName, [Float])]
                -> [(String, [Float])]
devTemplates disks mounted dat =
  map (\(d, p) -> (findTempl d p disks, findData d)) mounted
  where findData dev = case find ((==dev) . fst) dat of
                         Nothing -> [0, 0, 0]
                         Just (_, xs) -> xs
