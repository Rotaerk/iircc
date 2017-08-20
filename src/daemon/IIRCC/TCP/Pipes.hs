{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module IIRCC.TCP.Pipes (
  Socket,
  receiver,
  sender
) where

import Control.Monad
import qualified Control.Monad.Catch as C
import Data.ByteString
import Data.Function (fix)
import Data.Functor
import GHC.IO.Exception

import Pipes
import Pipes.Core
import Pipes.Exhaustion
import Pipes.Safe

import Network.Simple.TCP

receiver :: (MonadCatch io, MonadIO io) => Socket -> Int -> Server Int ByteString io ()
receiver socket = fix $ \receiver' numBytes ->
  liftIO (recv socket numBytes) >>= \case
    Just bs -> respond bs >>= receiver'
    Nothing -> return ()

sender :: MonadIO io => Socket -> Consumer ByteString io a
sender socket = inexhaustible $ forever $ await >>= liftIO . send socket
