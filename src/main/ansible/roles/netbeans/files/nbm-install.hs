#!/usr/bin/env runhaskell

import Control.Applicative
import Control.Monad

import Data.List
import Data.Monoid

import System.Directory
import System.Environment
import System.FilePath
import System.Posix.Files
import System.Process

netbeansHome :: FilePath
netbeansHome = "/usr/local/stow/netbeans"

main :: IO ()
main = do
  nbm <- nbmFile
  dir <- mkTempDir nbm
  extract nbm dir
  unpack dir
  copyToNetbeans dir
  -- read nbm file name from args
  -- unzip into tmp dir
  -- go through tmp dir and unpack pack.gz files
  -- copy to nebeans directory
  return ()

nbmFile :: IO FilePath
nbmFile = head <$> getArgs

mkTempDir :: FilePath -> IO FilePath
mkTempDir nbm = createDirectoryIfMissing True path >> return path
  where
    path = "/tmp" </> takeFileName nbm

extract :: FilePath -> FilePath -> IO ()
extract src dst = do
  (_,_,_,h) <- createProcess (proc "unzip" [src, "-d", dst])
  void $ waitForProcess h

data ToUnpack = Directory FilePath | PackGz FilePath | Ignore FilePath

mkToUnpack :: FilePath -> IO ToUnpack
mkToUnpack f = do
  status <- getFileStatus f
  isDir <- pure $ isDirectory status
  isHidden <- pure $ "." `isPrefixOf` takeFileName f
  isPackGz <- pure $ ".pack.gz" `isSuffixOf` f
  return $ make isDir isHidden isPackGz

  where
    make _ True _ = Ignore f
    make True _ _ = Directory f
    make _ _ True = PackGz f
    make _ _ _    = Ignore f

unpack :: FilePath -> IO ()
unpack f = do
  up <- mkToUnpack f
  case up of
    Directory d -> unpackDirectory d
    Ignore _    -> return ()
    PackGz pgz  -> unpack200 pgz

unpackDirectory :: FilePath -> IO ()
unpackDirectory d = getDirectoryContents d
  >>= pure . fmap (d </>)
  >>= mapM_ unpack

unpack200 :: FilePath -> IO ()
unpack200 pgz = do
  putStrLn $ "Extracing to " `mappend` jar 
  (_,_,_,h) <- createProcess (proc "unpack200" [pgz, jar])
  void $ waitForProcess h
  removeFile pgz
  where
    jar = dropExtensions pgz `mappend` ".jar"

copyToNetbeans :: FilePath -> IO ()
copyToNetbeans f = do
  (_,_,_,h) <- createProcess
    (proc "cp" ["-v", "-a" , f </> "netbeans/.", "/etc/skel/.netbeans/8.0/"])
  void $ waitForProcess h



