#!/usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Error

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

type MountBox = ErrorT String IO

runMountBox = runErrorT

mountPoint = "/mnt/box"
credentials = "/etc/samba/box.cifs.credentials"


main :: IO ()
main = runMountBox (run `catchError` failAndExit) >> exitSuccess

run :: (MonadIO m, MonadError String m) => m ()
run = do
  checkCredentialsFileExists
  checkNotAlreadyMounted

  mountCifs
  bindToShare


bindToShare :: (MonadIO m, MonadError String m) => m()
bindToShare = runAndWait cmd errorMessage
  where
    cmd = "mount --bind /mnt/box /srv/samba/share/box"
    errorMessage = "Bind to share failed"

mountCifs :: (MonadIO m, MonadError String m) => m()
mountCifs = runAndWait cmd errorMessage
  where
    cmd = "mount -t cifs //10.0.2.2/box /mnt/box -o credentials=" ++ credentials
    errorMessage = "Mount command failed"


checkCredentialsFileExists :: (MonadIO m, MonadError String m) => m ()
checkCredentialsFileExists =
  unlessM (liftIO $ doesFileExist credentials) (throwError errorMessage)
  where
    errorMessage = "Credentials file does not exist: " ++ credentials

checkNotAlreadyMounted :: (MonadIO m, MonadError String m) => m ()
checkNotAlreadyMounted = do
  es <- liftIO exitCode

  case es of
    ExitSuccess -> do
      throwError errorMessage
    _           -> return ()

  where
    exitCode = runCommand cmd >>= waitForProcess
    cmd = "mountpoint -q " ++ mountPoint
    errorMessage = "Already mounted: " ++ mountPoint

runAndWait :: (MonadIO m, MonadError String m) => String -> String -> m ()
runAndWait cmd err = do
  es <- liftIO exitCode

  case es of
    ExitFailure _ -> throwError err
    _             -> return ()

  where
    exitCode = runCommand cmd >>= waitForProcess

failAndExit :: (MonadIO m) => String -> m ()
failAndExit msg = liftIO $ hPutStrLn stderr msg >> exitFailure

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b m = b >>= flip unless m

