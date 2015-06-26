{-# OPTIONS_GHC -XPatternGuards -XFlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ViewDoc (toggleSaveState, launchDocuments) where

import Control.Monad
import qualified Data.ByteString.Char8 as Str

import XMonad
import XMonad.Core
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.SpawnOn
import XMonad.Util.Loggers

import System.Posix.Types
import System.Directory
import System.Path
import System.FilePath
import Data.Maybe
import Data.List

history :: String
history = ".viewedDocs"

toggleSaveState :: X ()
toggleSaveState = withFocused (runQuery pid >=> updateDoc)

procGetCmdline :: ProcessID -> IO String
procGetCmdline pid = (readFile filepath)
    where filepath = "/proc" </> show pid </> "cmdline"

updateDoc :: Maybe ProcessID -> X ()
updateDoc t = case t of
    Nothing -> return ()
    Just p ->  do 
      home <- io $ getHomeDirectory
      x <- io $ Str.readFile (fromJust $ absNormPath home history)
      cmdline <- io $ (procGetCmdline p)
      workspace <- logCurrent
      let commands = map (read :: String -> [String]) $ filter (/= "") $ lines $ Str.unpack x
      let index = findIndex (\ [cmd,_] -> cmd == cmdline) commands
      let new_commands = case index of
                             Just i -> ys ++ (tail zs)
                                 where (ys,zs) = splitAt i commands
                             Nothing -> [cmdline, fromJust workspace] : commands
      colorWindows p (not $ index == Nothing)
      io $ writeFile history (unlines $ map show new_commands)

colorWindows :: ProcessID -> Bool -> X ()
colorWindows p True  = do 
  withFocused $ \w -> setWindowBorder' "blue" w
colorWindows p False = do
  withFocused $ \w -> setWindowBorder' "green" w

setWindowBorder' :: (MonadIO m, MonadReader XConf m) => String -> Window -> m ()
setWindowBorder' c w = do
    XConf { display = d } <- ask
    ~(Just pc) <- io $ initColor d c
    io $ setWindowBorder d w pc

launchDocuments :: X ()
launchDocuments = do
  home <- io $ getHomeDirectory
  x <- io $ Str.readFile (fromJust $ absNormPath home history)
  mapM_ launchFile (lines $ Str.unpack x)

launchFile :: String -> X ()
launchFile "" = return ()
launchFile f  = launchFile' (read f)
  where launchFile' :: [String] -> X ()
        launchFile' [cmd,workspace] = spawnOn workspace cmd
