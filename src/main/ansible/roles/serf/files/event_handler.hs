#!/usr/bin/env runhaskell

import Control.Applicative
import Control.Monad

import Data.List
import Data.Monoid

import System.Directory
import System.Exit
import System.IO
import System.Posix.Files
import System.Process

eventHandlersDirectory :: FilePath
eventHandlersDirectory = "/etc/serf/event_handler.d/"

main :: IO ()
main = do
  inp <- getLine
  result <- mconcat <$> (handlers >>= mapM (run inp))
  exit result

handlers :: IO [String]
handlers = fmap sort $ allFiles >>= filterM isFile
  where
    allFiles = fmap makeAbsPath <$> getDirectoryContents eventHandlersDirectory
    makeAbsPath = mappend eventHandlersDirectory

run :: String -> FilePath -> IO HandlerResult
run inp hdlr = do
  putStr $ mconcat ["Running ", hdlr, "..."]

  (Just hin, _, _, ps) <-
    createProcess (proc hdlr [])
        { cwd = Just eventHandlersDirectory
        , std_in = CreatePipe
        }

  hPutStr hin inp

  exitCode <- waitForProcess ps
  print exitCode
  return $ fromExitCode exitCode

isFile :: FilePath -> IO Bool
isFile = fmap isRegularFile . getFileStatus

data HandlerResult = Success | Failure

fromExitCode :: ExitCode -> HandlerResult
fromExitCode ExitSuccess     = Success
fromExitCode (ExitFailure _) = Failure

exit :: HandlerResult -> IO ()
exit Success = exitSuccess
exit Failure = exitFailure

instance Monoid HandlerResult where
  mempty = Success
  mappend Success r = r
  mappend Failure _ = Failure
  

