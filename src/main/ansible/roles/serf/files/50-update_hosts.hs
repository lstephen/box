#!/usr/bin/env runhaskell

import Control.Applicative
import Control.Monad

import Data.Monoid

import Serf.Event
import Serf.Member

hostsFile :: FilePath
hostsFile = "/etc/hosts.d/serf.hosts"

main :: IO ()
main = handleEventWith handler

handler :: EventHandler IO
handler evt = when (isMemberEvent evt) updateHosts

updateHosts :: IO ()
updateHosts = toHosts <$> members >>= write hostsFile

toHosts :: [Member] -> [Host]
toHosts = fmap toHost
  where
    toHost m = Host (ipAddress m) (name m)

write :: FilePath -> [Host] -> IO ()
write f hs = writeFile f . unlines $ formatHost <$> hs

data Host = Host IpAddress String

formatHost :: Host -> String
formatHost (Host i n) = mconcat [i, " ", n]


