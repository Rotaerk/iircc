{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.IIRCC where

import Codec.CBOR.Read
import Codec.CBOR.Write
import Codec.Serialise
import Codec.Serialise.Encoding
import Codec.Serialise.Decoding
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Dependent.Sum
import Data.Either.Combinators
import Data.Functor.Identity
import Data.Monoid
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

instance Serialise ClientCommand

data SessionCommand =
  Connect |
  Join ChannelName |
  --ToChannel ChannelName ChannelCommand |
  SendQuit QuitMessage |
  Disconnect |
  EndSession
  deriving (Show, Generic)

instance Serialise SessionCommand

--data ChannelCommand =
--  SendMessage Text |
--  Part

data ClientEventTag f a where
  SessionStarted :: ClientEventTag f SessionName
  FromSession :: f (SessionName, f (SessionEventTag f a)) -> ClientEventTag f a
  ClientUnableTo :: ClientEventTag f (ClientCommand, ErrorMessage)
  ClientEnding :: ClientEventTag f ()

type ClientEventFilter = ClientEventTag Maybe
type ClientEventHeader = ClientEventTag Identity
type ClientEvent = DSum ClientEventHeader Identity

data SessionEventTag (f :: * -> *) a where
  Connecting :: SessionEventTag f (HostName, PortNumber)
  Connected :: SessionEventTag f ()
  FailedToConnect :: SessionEventTag f ErrorMessage
  --FromChannel :: ChannelName -> ChannelEventTag a -> SessionEventTag a
  ReceivedUninterpretedMessage :: SessionEventTag f UninterpretedMessage
  SessionUnableTo :: SessionEventTag f (SessionCommand, ErrorMessage)
  SentQuit :: SessionEventTag f QuitMessage
  EndedConnection :: SessionEventTag f ()
  LostConnection :: SessionEventTag f (Maybe ErrorMessage)
  EndedSession :: SessionEventTag f ()

type SessionEventFilter = SessionEventTag Maybe
type SessionEventHeader = SessionEventTag Identity
type SessionEvent = DSum SessionEventHeader Identity

--data ChannelEventTag a where
--  ReceivedMessage :: ChannelEventTag Text
--  SentMessage :: ChannelEventTag Text

{-
data ChannelEventTag a where
-}

-- The encoding of a filter needs to be a prefix of a valid header encoding.
encodeClientEventFilter :: ClientEventFilter a -> Encoding
encodeClientEventFilter = \case
  SessionStarted -> encodeTag 0 <> encodeNull
  FromSession Nothing -> encodeTag 1
  FromSession (Just (sessionName, Nothing)) -> encodeTag 1 <> encodeString sessionName <> encodeNull
  FromSession (Just (sessionName, Just sessionEventFilter)) -> encodeTag 1 <> encodeString sessionName <> encodeNull <> encodeSessionEventFilter sessionEventFilter
  ClientUnableTo -> encodeTag 2 <> encodeNull
  ClientEnding -> encodeTag 3 <> encodeNull

encodeSessionEventFilter :: SessionEventFilter a -> Encoding
encodeSessionEventFilter = \case
  Connecting -> encodeTag 0 <> encodeNull
  Connected -> encodeTag 1 <> encodeNull
  FailedToConnect -> encodeTag 2 <> encodeNull
  --FromChannel -> encodeTag 3
  ReceivedUninterpretedMessage -> encodeTag 4 <> encodeNull
  SessionUnableTo -> encodeTag 5 <> encodeNull
  SentQuit -> encodeTag 6 <> encodeNull
  EndedConnection -> encodeTag 7 <> encodeNull
  LostConnection -> encodeTag 8 <> encodeNull
  EndedSession -> encodeTag 9 <> encodeNull

encodeClientEvent :: ClientEvent -> (Encoding, Encoding)
encodeClientEvent = \case
  SessionStarted :=> Identity v -> (encodeTag 0 <> encodeNull, encode v)
  FromSession (Identity (sessionName, Identity sessionEventHeader)) :=> Identity v ->
    mapFst (mappend $ encodeTag 1 <> encodeString sessionName <> encodeNull) $ encodeSessionEvent (sessionEventHeader ==> v)
  ClientUnableTo :=> Identity v -> (encodeTag 2 <> encodeNull, encode v)
  ClientEnding :=> Identity v -> (encodeTag 3 <> encodeNull, encode v)

  where
    mapFst f (a, b) = (f a, b)

encodeSessionEvent :: SessionEvent -> (Encoding, Encoding)
encodeSessionEvent = \case
  Connecting :=> Identity v -> (encodeTag 0 <> encodeNull, encode v)
  Connected :=> Identity v -> (encodeTag 1 <> encodeNull, encode v)
  FailedToConnect :=> Identity v -> (encodeTag 2 <> encodeNull, encode v)
  ReceivedUninterpretedMessage :=> Identity v -> (encodeTag 3 <> encodeNull, encode v)
  SessionUnableTo :=> Identity v -> (encodeTag 4 <> encodeNull, encode v)
  SentQuit :=> Identity v -> (encodeTag 5 <> encodeNull, encode v)
  EndedConnection :=> Identity v -> (encodeTag 6 <> encodeNull, encode v)
  LostConnection :=> Identity v -> (encodeTag 7 <> encodeNull, encode v)
  EndedSession :=> Identity v -> (encodeTag 8 <> encodeNull, encode v)

decodeDSumWithTag :: (Serialise a, Applicative f) => tag a -> Decoder s (DSum tag f)
decodeDSumWithTag tag = (tag ==>) <$> decode

decodeClientEvent :: Decoder s (Decoder s ClientEvent)
decodeClientEvent = decodeTag >>= \case
  0 -> decodeTrivial SessionStarted
  1 -> do
    sessionName <- decodeString
    decodeNull
    decodeSessionEvent <&.&> \(sessionEventHeader :=> v) ->
      FromSession (Identity (sessionName, Identity sessionEventHeader)) :=> v
  2 -> decodeTrivial ClientUnableTo
  3 -> decodeTrivial ClientEnding
  _ -> undefined

  where
    (<$.$>) = (<$>) . (<$>)
    (<&.&>) = flip (<$.$>)
    decodeTrivial tag = decodeNull >> return (decodeDSumWithTag tag)

decodeSessionEvent :: Decoder s (Decoder s SessionEvent)
decodeSessionEvent = decodeTag >>= \case
  0 -> decodeTrivial Connecting
  1 -> decodeTrivial Connected
  2 -> decodeTrivial FailedToConnect
  3 -> decodeTrivial ReceivedUninterpretedMessage
  4 -> decodeTrivial SessionUnableTo
  5 -> decodeTrivial SentQuit
  6 -> decodeTrivial EndedConnection
  7 -> decodeTrivial LostConnection
  8 -> decodeTrivial EndedSession
  _ -> undefined

  where
    decodeTrivial tag = decodeNull >> return (decodeDSumWithTag tag)
