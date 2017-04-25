{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Network.IIRCC (
  IrcCommand (..),
  IrcEvent (..)
) where

import Data.ByteString
import Data.Serialize
import GHC.Generics
import Network.Simple.TCP

data IrcCommand =
  SendMessage ByteString
  deriving (Show, Generic, Eq)

instance Serialize IrcCommand

data IrcEvent =
  Connected HostName ServiceName |
  FailedToConnect HostName ServiceName |
  ClosedConnection |
  LostConnection 
  deriving (Show, Generic, Eq)

instance Serialize IrcEvent
