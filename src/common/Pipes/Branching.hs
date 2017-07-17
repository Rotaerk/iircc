{-# LANGUAGE LambdaCase #-}

module Pipes.Branching (
  branch,
  remap
) where

import Data.Functor.Identity

import Pipes
import Pipes.Extras
import qualified Pipes.Prelude as PP

branch :: Monad m => (i -> Maybe i') -> Pipe i' o m r -> Pipe i o m r -> Pipe i o m r
branch tryBranch dp p =
  PP.map (\i -> maybe (Left i) Right (tryBranch i)) >->
  (p +++ dp) >->
  PP.map (\case { Left o -> o; Right o -> o })

remap :: Monad m => (i -> Maybe o) -> Pipe i o m r -> Pipe i o m r
remap tryRemap p = branch tryRemap cat p
