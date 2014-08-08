#!/usr/bin/env runhaskell

import Control.Applicative
import Control.Monad

import Data.List
import Data.Monoid

import System.Directory
import Serf.Event
import Serf.Member

import System.Exit
import System.Process

main :: IO ()
main = handleEventWith handler

handler :: EventHandler IO
handler (MemberLeave m)  = unmountStore m >> mountStores
handler (MemberFailed m) = unmountStore m >> mountStores
handler e                = when (isMemberEvent e) mountStores

unmountStore :: Member -> IO ()
unmountStore m = do
  mtd <- isMounted m
  when mtd . void . umount $ getMountPoint m

mountStores :: IO ()
mountStores = filter isStore <$> members >>= mapM_ mountStore

isStore :: Member -> Bool
isStore m = "role=store" `isInfixOf` tags m

mountStore :: Member -> IO ()
mountStore m = createDirectoryIfMissing True (getMountPoint m)
    >> void (mount (name m) (getMountPoint m))

getMountPoint :: Member -> FilePath
getMountPoint m = "/mnt/" `mappend` name m

isMounted :: Member -> IO Bool
isMounted m = mountpoint $ getMountPoint m

--
-- Raw mount/umount/mountpoint commands
--
umount :: FilePath -> IO ExitCode
umount mnt = spawnProc "sudo" ["umount.glusterfs", "-f", "-l", mnt] >>= waitForProcess

-- TODO: Log failures of the mount command. Is this done by mount itself?
mount :: String -> FilePath -> IO ExitCode
mount host mnt = spawn >>= waitForProcess
  where
    spawn = spawnProc "sudo" ["mount.glusterfs", share ,mnt]
    share = mconcat [host, ":/box"]

mountpoint :: FilePath -> IO Bool
mountpoint mnt = (==) ExitSuccess <$> runMountpoint
  where
    runMountpoint = spawnProc "mountpoint" ["-q", mnt] >>= waitForProcess

spawnProc :: FilePath -> [String] -> IO ProcessHandle
spawnProc cmd args = createProcess (proc cmd args) >>= \(_,_,_,h) -> return h

