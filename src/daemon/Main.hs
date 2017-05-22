{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Applicative
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString as ABS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Tagged
import Data.Text
import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.Attoparsec as PA

import Irc.Commands
import Irc.Message
import Irc.RawIrcMsg

import qualified IRC

-- Run a test IRC daemon with:  ngircd -f ~/ngircd.conf -n

main :: IO ()
main = do
  c <- IRC.connect "localhost" "6667"
  runEffect $ do
    IRC.toConnection c <-< do
      IRC.send $ ircNick "rotaerk"
      IRC.send $ ircUser "rotaerk" False False "Matt"
    for (IRC.fromConnection c) $ liftIO . print
    IRC.toConnection c <-< IRC.close
  wait $ IRC.connectionTask c
