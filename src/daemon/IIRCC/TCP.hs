{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module IIRCC.TCP (
  Socket,
  receiver,
  sender
) where

import Control.Monad
import Data.ByteString
import Data.Function (fix)
import Data.Functor

import Pipes
import Pipes.Core
import Pipes.Safe

import Network.Simple.TCP

receiver :: Socket -> Int -> Server Int ByteString IO (Maybe IOError)
receiver socket = fix $ \receiver' numBytes ->
  do
    recv socket numBytes >>= \case
      Just bs -> respond bs >>= receiver'
      Nothing -> return Nothing
  `catchIOError`
  (return . Just)

sender :: Socket -> Consumer ByteString IO ()
sender socket = forever $ await >>= send socket
