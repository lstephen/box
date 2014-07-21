#!/usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Error

import Data.Monoid

import System.Directory
import System.Exit
import System.IO
import System.Process

type MountBox = ErrorT String IO

runMountBox :: MountBox a -> IO (Either String a)
runMountBox = runErrorT

mountPoint :: FilePath
mountPoint = "/mnt/box"

credentials :: FilePath
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
    cmd = "mount -t cifs //10.0.2.2/box /mnt/box -o credentials=" `mappend` credentials `mappend` ",file_mode=0666,dir_mode=0777"
    errorMessage = "Mount command failed"


checkCredentialsFileExists :: (MonadIO m, MonadError String m) => m ()
checkCredentialsFileExists =
  unlessM (liftIO $ doesFileExist credentials) (throwError errorMessage)
  where
    errorMessage = "Credentials file does not exist: " `mappend` credentials

checkNotAlreadyMounted :: (MonadIO m, MonadError String m) => m ()
checkNotAlreadyMounted = do
  es <- liftIO exitCode

  case es of
    ExitSuccess -> do
      throwError errorMessage
    _           -> return ()

  where
    exitCode = runCommand cmd >>= waitForProcess
    cmd = "mountpoint -q " `mappend` mountPoint
    errorMessage = "Already mounted: " `mappend` mountPoint

runAndWait :: (MonadIO m, MonadError String m) => String -> String -> m ()
runAndWait cmd err = do
  liftIO . putStrLn $ mconcat ["Running ", cmd, "..."]
  es <- liftIO exitCode
  liftIO . putStrLn $ mconcat [cmd, " Done. ", show es]

  case es of
    ExitFailure _ -> throwError err
    _             -> return ()

  where
    exitCode = runCommand cmd >>= waitForProcess

failAndExit :: (MonadIO m) => String -> m ()
failAndExit msg = liftIO $ hPutStrLn stderr msg >> exitFailure

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b m = b >>= flip unless m

