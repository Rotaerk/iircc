{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

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
  FromSessionF :: f (SessionName, f (SessionEventTag f a)) -> ClientEventTag f a
  ClientUnableTo :: ClientEventTag f (ClientCommand, ErrorMessage)
  ClientEnding :: ClientEventTag f ()

pattern FromSession sessionName sessionEventTag = FromSessionF (Identity (sessionName, Identity sessionEventTag))

type ClientEventFilter = ClientEventTag Maybe
type ClientEventHeader = ClientEventTag Identity
type ClientEvent = DSum ClientEventHeader Identity

pattern SessionStartedTagNum = 0
pattern FromSessionTagNum = 1
pattern ClientUnableToTagNum = 2
pattern ClientEndingTagNum = 3

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

pattern ConnectingTagNum = 0
pattern ConnectedTagNum = 1
pattern FailedToConnectTagNum = 2
--pattern FromChannelTagNum = 3
pattern ReceivedUninterpretedMessageTagNum = 4
pattern SessionUnableToTagNum = 5
pattern SentQuitTagNum = 6
pattern EndedConnectionTagNum = 7
pattern LostConnectionTagNum = 8
pattern EndedSessionTagNum = 9

--data ChannelEventTag a where
--  ReceivedMessage :: ChannelEventTag Text
--  SentMessage :: ChannelEventTag Text

{-
data ChannelEventTag a where
-}

-- This looks generalized enough to handle any foldable structure, but it will ultimately
-- only be needed for Identity (1 element) or Maybe (0 or 1 element).  Untested for other
-- foldable types.  Implementing this way just saves us from code duplication.
encodeClientEventTag :: Foldable f => ClientEventTag f a -> Encoding
encodeClientEventTag = (<> encodeNull) . \case
  SessionStarted -> encodeTag SessionStartedTagNum
  FromSessionF details ->
    encodeTag FromSessionTagNum <> (
      foldFor details $ \(sessionName, sessionEventTags) ->
        encodeString sessionName <> foldFor sessionEventTags encodeSessionEventTag
    )
  ClientUnableTo -> encodeTag ClientUnableToTagNum
  ClientEnding -> encodeTag ClientEndingTagNum

  where
    foldFor = flip foldMap

encodeSessionEventTag :: SessionEventTag f a -> Encoding
encodeSessionEventTag = \case
  Connecting -> encodeTag ConnectingTagNum <> encodeNull
  Connected -> encodeTag ConnectedTagNum <> encodeNull
  FailedToConnect -> encodeTag FailedToConnectTagNum <> encodeNull
  --FromChannel -> encodeTag FromChannelTagNum <> encodeNull
  ReceivedUninterpretedMessage -> encodeTag ReceivedUninterpretedMessageTagNum <> encodeNull
  SessionUnableTo -> encodeTag SessionUnableToTagNum <> encodeNull
  SentQuit -> encodeTag SentQuitTagNum <> encodeNull
  EndedConnection -> encodeTag EndedConnectionTagNum <> encodeNull
  LostConnection -> encodeTag LostConnectionTagNum <> encodeNull
  EndedSession -> encodeTag EndedSessionTagNum <> encodeNull

encodeClientEventData :: ClientEvent -> Encoding
encodeClientEventData = \case
  SessionStarted :=> Identity v -> encode v
  FromSession _ sessionEventHeader :=> Identity v -> encodeSessionEventData (sessionEventHeader ==> v)
  ClientUnableTo :=> Identity v -> encode v
  ClientEnding :=> Identity v -> encode v

encodeSessionEventData :: SessionEvent -> Encoding
encodeSessionEventData = \case
  Connecting :=> Identity v -> encode v
  Connected :=> Identity v -> encode v
  FailedToConnect :=> Identity v -> encode v
  --FromChannel :=> Identity v -> encode v
  ReceivedUninterpretedMessage :=> Identity v -> encode v
  SessionUnableTo :=> Identity v -> encode v
  SentQuit :=> Identity v -> encode v
  EndedConnection :=> Identity v -> encode v
  LostConnection :=> Identity v -> encode v
  EndedSession :=> Identity v -> encode v

decodeDSumWithTag :: (Serialise a, Applicative f) => tag a -> Decoder s (DSum tag f)
decodeDSumWithTag tag = (tag ==>) <$> decode

decodeClientEvent :: Decoder s (Decoder s ClientEvent)
decodeClientEvent = decodeTag >>= \case
  SessionStartedTagNum -> decodeTrivial SessionStarted
  FromSessionTagNum -> do
    sessionName <- decodeString
    decodeNull
    decodeSessionEvent <&.&> \(sessionEventHeader :=> v) -> FromSession sessionName sessionEventHeader :=> v
  ClientUnableToTagNum -> decodeTrivial ClientUnableTo
  ClientEndingTagNum -> decodeTrivial ClientEnding
  _ -> undefined

  where
    (<$.$>) = (<$>) . (<$>)
    (<&.&>) = flip (<$.$>)
    decodeTrivial tag = decodeNull >> return (decodeDSumWithTag tag)

decodeSessionEvent :: Decoder s (Decoder s SessionEvent)
decodeSessionEvent = decodeTag >>= \case
  ConnectingTagNum -> decodeTrivial Connecting
  ConnectedTagNum -> decodeTrivial Connected
  FailedToConnectTagNum -> decodeTrivial FailedToConnect
  ReceivedUninterpretedMessageTagNum -> decodeTrivial ReceivedUninterpretedMessage
  SessionUnableToTagNum -> decodeTrivial SessionUnableTo
  SentQuitTagNum -> decodeTrivial SentQuit
  EndedConnectionTagNum -> decodeTrivial EndedConnection
  LostConnectionTagNum -> decodeTrivial LostConnection
  EndedSessionTagNum -> decodeTrivial EndedSession
  _ -> undefined

  where
    decodeTrivial tag = decodeNull >> return (decodeDSumWithTag tag)
