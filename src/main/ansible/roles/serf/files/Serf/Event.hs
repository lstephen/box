module Serf.Event where

import Serf.Member

import Control.Applicative
import Control.Monad.IO.Class

import System.Environment
import System.Exit
import System.IO

import Text.Parsec

type SerfError = String

data SerfEvent = MemberJoin Member
               | MemberLeave Member
               | MemberFailed Member
               | MemberUpdate Member
               | MemberReap Member
               | User
               | Query
               | Unknown String


getSerfEvent :: IO (Either SerfError SerfEvent)
getSerfEvent = getEnv "SERF_EVENT" >>= fromString
  where
    fromString :: String -> IO (Either SerfError SerfEvent)
    fromString "member-join" = readMemberEvent MemberJoin
    fromString "member-leave" = readMemberEvent MemberLeave
    fromString "member-failed" = readMemberEvent MemberFailed
    fromString "member-update" = readMemberEvent MemberUpdate
    fromString "member-reap" = readMemberEvent MemberReap
    fromString "user"  = return $ Right User
    fromString "query" = return $ Right Query
    fromString unk     = return . Right $ Unknown unk

    readMemberEvent :: (Member -> SerfEvent) -> IO (Either SerfError SerfEvent)
    readMemberEvent f = addMember f <$> readMember

    addMember :: (Member -> SerfEvent)
              -> Either ParseError Member
              -> Either SerfError SerfEvent
    addMember _ (Left err) = Left $ show err
    addMember f (Right m)  = Right $ f m

    readMember :: IO (Either ParseError Member)
    readMember = memberFromString <$> getLine


isMemberEvent :: SerfEvent -> Bool
isMemberEvent (MemberJoin _)   = True
isMemberEvent (MemberLeave _)  = True
isMemberEvent (MemberFailed _) = True
isMemberEvent (MemberUpdate _) = True
isMemberEvent (MemberReap _)   = True
isMemberEvent _                = False


type EventHandler m = SerfEvent -> m ()

handleEventWith :: MonadIO m => EventHandler m -> m ()
handleEventWith hdlr = do
  evt <- liftIO getSerfEvent
  case evt of
    Left err -> liftIO $ hPutStrLn stderr err >> exitFailure
    Right ev -> hdlr ev


