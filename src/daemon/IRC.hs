module IRC where

import Data.ByteString
import Data.List.NonEmpty
import Data.These

import Network.IRC.Parser

data Event = Received Message | Disconnected | SessionEnded deriving (Show)

data Message = Message (Maybe Prefix) Command [Param] deriving (Show)

data Prefix =
  PrefixServerName ServerName |
  PrefixNickName NickName (Maybe (Maybe User, Host))
  deriving (Show)

{-
data Target =
  TargetNickName NickName |
  TargetServerName ServerName
  deriving (Show)

type MsgTarget = NonEmpty MsgTo

data MsgTo =
  MsgToChannel Channel |
  MsgToUser User (These Host ServerName) |
  MsgToTargetMask TargetMask |
  MsgToNickName NickName (Maybe (User, Host))
  deriving (Show)
-}
