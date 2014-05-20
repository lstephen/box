#!/usr/bin/env runhaskell

import Data.Monoid

import System.Process

main = do
  evt <- getSerfEvent
  when (isMemberLeaveOrFail evt) $ do
    
  -- If is member leave or fail, then umount
  -- If is a member event
  --   loop through all that are stores and mount them


data SerfEvent = SerfEvent evt

instance IsString SerfEvent where
  fromString = SerfEvent

getSerfEvent :: IO SerfEvent
getSerfEvent = fromString <$> getEnv "SERF_EVENT"

isMemberEvent :: SerfEvent -> Bool
isMemberEvent (SerfEvent evt) = "member" `isPrefixOf` evt

isMemberLeaveOrFail :: SerfEvent -> Bool
isMemberLeaveOrFail (SerfEvent evt) = evt == "member-leave" || evt == "member-fail"


--
-- Raw mount/umount/mountpoint commands
--
umount :: FilePath -> IO ()
umount mnt = runCommand ("umount.cifs -f -l " ++ mnt) >>= waitForProcess

mount :: String -> FilePath -> IO ()
mount host mnt = runCommand command >>= waitForProcess
  where
    command = mconcat ["mount.cifs", "-o guest", share, " ", mnt]
    share = mconcat ["//", host, "/export"]

mountpoint :: FilePath -> IO Bool
mountpoint mnt = do
  p <- runCommand $ "mountpoint -q " ++ mnt
  exitCode <- waitForProcess p
  return exitCode == 0

