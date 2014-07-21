#!/usr/bin/env runhaskell

import Control.Concurrent
import Control.Monad

import Data.Monoid

import System.Exit
import System.Process

remote :: String
remote = "rsync://10.0.2.2/box"

local :: String
local = "/data/box"

main :: IO ExitCode
main = do
  pull
  forever $ do
    threadDelay oneMinuteInMilliSeconds
    push

oneMinuteInMilliSeconds :: Int
oneMinuteInMilliSeconds = 1000 * 60

pull :: IO ExitCode
pull = rsync ["--chmod=Da=rwx,Fa=rw"] remote local

push :: IO ExitCode
push = rsync [] local remote

rsync :: [String] -> String -> String -> IO ExitCode
rsync args src dest = do
  (_,_,_,h) <- createProcess $ proc prog args
  waitForProcess h
  where
    prog = "rsync"
    args = ["-v", "-u", "-a"] `mappend` args : remote : local


