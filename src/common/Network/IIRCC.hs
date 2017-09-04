{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.IIRCC where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Serialize
import Data.Serialize.Text
import Data.Text
import Data.Word
import GHC.Generics

type ChannelName = Text
type SessionName = Text
type QuitMessage = Text
type UninterpretedMessage = Text
type ErrorMessage = Text
type HostName = Text
type PortNumber = Word16

data ClientCommand =
  StartSession SessionName HostName PortNumber |
  ToSession SessionName SessionCommand |
  EndClient
  deriving (Show, Generic)

instance Serialize ClientCommand

data SessionCommand =
  Connect |
  Join ChannelName |
  --ToChannel ChannelName ChannelCommand |
  SendQuit QuitMessage |
  Disconnect |
  EndSession
  deriving (Show, Generic)

instance Serialize SessionCommand

--data ChannelCommand =
--  SendMessage Text |
--  Part

data ClientEventTag a where
  SessionStarted :: ClientEventTag SessionName
  FromSession :: SessionName -> SessionEventTag a -> ClientEventTag a
  ClientUnableTo :: ClientEventTag (ClientCommand, ErrorMessage)
  ClientEnding :: ClientEventTag ()

type ClientEvent = DSum ClientEventTag Identity

data SessionEventTag a where
  Connecting :: SessionEventTag (HostName, PortNumber)
  Connected :: SessionEventTag ()
  FailedToConnect :: SessionEventTag ErrorMessage
  --FromChannel :: ChannelName -> ChannelEventTag a -> SessionEventTag a
  ReceivedUninterpretedMessage :: SessionEventTag UninterpretedMessage
  SessionUnableTo :: SessionEventTag (SessionCommand, ErrorMessage)
  SentQuit :: SessionEventTag QuitMessage
  EndedConnection :: SessionEventTag ()
  LostConnection :: SessionEventTag (Maybe ErrorMessage)
  EndedSession :: SessionEventTag ()

type SessionEvent = DSum SessionEventTag Identity

--data ChannelEventTag a where
--  ReceivedMessage :: ChannelEventTag Text
--  SentMessage :: ChannelEventTag Text

{-
data ChannelEventTag a where
-}

{-
encodeClientEvent :: ClientEvent -> (ByteString, ByteString)
encodeClientEvent = \case
  (SessionStarted :=> Identity v) -> ("SessionStarted.", encode v)
  (FromSession sessionName sessionEventTag :=> Identity v) ->
    let (sessionEventTagBS, sessionEventValueBS) = encodeSessionEvent (sessionEventTag ==> v)
    (concat ["FromSession:", ".", encode v)
  (ClientUnableTo :=> Identity v) -> ("ClientUnableTo.", encode v)
  (ClientEnding :=> Identity v) -> ("ClientEnding.", encode v)
-}

{-
encodeClientEventTag :: ClientEventTag a -> ByteString
encodeClientEventTag = \case
  SessionStarted -> "SessionStarted."
  FromSession sessionName sessionEventTag -> BS.concat ["FromSession:", encode sessionName, ".", encodeSessionEventTag sessionEventTag]
  ClientUnableTo -> "ClientUnableTo."
  ClientEnding -> "ClientEnding."
-}

-- TODO:  Use serialize package instead of cereal

encodeClientEvent :: ClientEvent -> ByteString
encodeClientEvent = \case
  (SessionStarted :=> Identity v) -> encodeAndTagWith "SessionStarted." v
  (FromSession sessionName sessionEventTag :=> Identity v) ->
    BS.concat ["FromSession:", encode sessionName, ".", encodeSessionEvent (sessionEventTag ==> v)]
  (ClientUnableTo :=> Identity v) -> encodeAndTagWith "ClientUnableTo." v
  (ClientEnding :=> Identity v) -> encodeAndTagWith "ClientEnding." v
  
  where
    encodeAndTagWith :: Serialize a => ByteString -> a -> ByteString
    encodeAndTagWith t = BS.append t . encode

encodeSessionEventTag :: SessionEventTag a -> ByteString
encodeSessionEventTag = \case
  Connecting -> "Connecting."
  Connected -> "Connected."
  FailedToConnect -> "FailedToConnect."
  ReceivedUninterpretedMessage -> "ReceivedUninterpretedMessage."
  SessionUnableTo -> "SessionUnableTo."
  SentQuit -> "SentQuit."
  EndedConnection -> "EndedConnection."
  LostConnection -> "LostConnection."
  EndedSession -> "EndedSession."

encodeSessionEventData :: SessionEvent -> ByteString
encodeSessionEventData = \case
  (Connecting :=> Identity v) -> encode v
  (Connected :=> Identity v) -> encode v
  (FailedToConnect :=> Identity v) -> encode v
  (ReceivedUninterpretedMessage :=> Identity v) -> encode v
  (SessionUnableTo :=> Identity v) -> encode v
  (SentQuit :=> Identity v) -> encode v
  (EndedConnection :=> Identity v) -> encode v
  (LostConnection :=> Identity v) -> encode v
  (EndedSession :=> Identity v) -> encode v

encodeSessionEvent :: SessionEvent -> ByteString
encodeSessionEvent = \case
  (Connecting :=> Identity v) -> encodeAndTagWith "Connecting." v
  (Connected :=> Identity v) -> encodeAndTagWith "Connected." v
  (FailedToConnect :=> Identity v) -> encodeAndTagWith "FailedToConnect." v
  (ReceivedUninterpretedMessage :=> Identity v) -> encodeAndTagWith "ReceivedUninterpretedMessage." v
  (SessionUnableTo :=> Identity v) -> encodeAndTagWith "SessionUnableTo." v
  (SentQuit :=> Identity v) -> encodeAndTagWith "SentQuit." v
  (EndedConnection :=> Identity v) -> encodeAndTagWith "EndedConnection." v
  (LostConnection :=> Identity v) -> encodeAndTagWith "LostConnection." v
  (EndedSession :=> Identity v) -> encodeAndTagWith "EndedSession." v
  
  where
    encodeAndTagWith :: Serialize a => ByteString -> a -> ByteString
    encodeAndTagWith t = BS.append t . encode

decodeSessionEvent :: ByteString -> ByteString -> Either String SessionEvent
decodeSessionEvent = \case
  "Connecting." -> decodeAndTagWith Connecting
  "Connected." -> decodeAndTagWith Connected
  "FailedToConnect." -> decodeAndTagWith FailedToConnect
  "ReceivedUninterpretedMessage." -> decodeAndTagWith ReceivedUninterpretedMessage
  "SessionUnableTo." -> decodeAndTagWith SessionUnableTo
  "SentQuit." -> decodeAndTagWith SentQuit
  "EndedConnection." -> decodeAndTagWith EndedConnection
  "LostConnection." -> decodeAndTagWith LostConnection
  "EndedSession." -> decodeAndTagWith EndedSession
  tagBS -> const $ Left $ "Unexpected tag: " ++ BS8.unpack tagBS

  where
    decodeAndTagWith :: Serialize a => SessionEventTag a -> ByteString -> Either String SessionEvent
    decodeAndTagWith t = fmap (t ==>) . decode
