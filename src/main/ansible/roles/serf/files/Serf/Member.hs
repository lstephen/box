module Serf.Member where

import Control.Applicative ((<$>))

import Data.Maybe
import Data.String

import System.IO
import System.Process

import Text.Parsec

type Name = String
type IpAddress = String
type Port = String

data Member = Member
  { name :: Name
  , ipAddress :: IpAddress
  , port :: Maybe Port
  , status :: Status
  }

data Status = StatusAlive | StatusLeft | StatusFailed | StatusUnknown
  deriving Eq

instance IsString Status where
  fromString "alive"  = StatusAlive
  fromString "left"   = StatusLeft
  fromString "failed" = StatusFailed
  fromString _       =  StatusUnknown


members :: IO [Member]
members = do
  output <- readProcess "serf" ["members"] ""
  filter (isStatus StatusAlive) <$> catMaybes <$> mapM fromParsed (readMembers output)
  where
    readMembers :: String -> [Either ParseError Member]
    readMembers = fmap memberFromString . lines

    fromParsed :: Either ParseError Member -> IO (Maybe Member)
    fromParsed (Right m) = return $ Just m
    fromParsed (Left err) = do
      hPrint stderr err
      return Nothing

isStatus :: Status -> Member -> Bool
isStatus s m = status m == s

memberFromString :: String -> Either ParseError Member
memberFromString s = parse parser s s
    where
      parser :: Parsec String () Member
      parser = do
        n <- many1 letter
        whitespace
        ip <- many1 (digit <|> char '.')
        p <- optionMaybe $ char ':' >> many1 digit
        whitespace
        st <- fromString <$> many1 letter
        return $ Member n ip p st

      whitespace = skipMany (space <|> tab)
