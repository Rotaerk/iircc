{-# LANGUAGE LambdaCase #-}

module Pipes.Partial (
  toPartial,
  partialPipe,
  partialMap,
  completeWith,
  complete
) where

import Pipes
import qualified Pipes.Prelude as PP

toPartial :: Monad m => Pipe i (Either i o) m r
toPartial = PP.map Left

partialPipe :: Monad m => (i -> Maybe a) -> Pipe a o m r -> Pipe (Either i o) (Either i o) m r
partialPipe tryMap p = await' >~ for p (yield . Right)
  where
    await' = await >>= \case
      l@(Left i) -> maybe (yield l >> await') return (tryMap i)
      r -> yield r >> await'

partialMap :: Monad m => (i -> Maybe o) -> Pipe (Either i o) (Either i o) m r
partialMap tryMap = PP.map $ either (\i -> maybe (Left i) Right $ tryMap i) Right

completeWith :: Monad m => (i -> o) -> Pipe (Either i o) o m r
completeWith mapDef = PP.map $ either mapDef id

complete :: Monad m => Pipe (Either i o) o m r
complete = PP.map $ \(Right o) -> o
