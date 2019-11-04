module Space.Frp.Graph
  ( makeGraphB
  ) where

import Reactive.Banana

import Space.Graph

makeGraphB :: MonadMoment m => Graph loc -> m (Behavior (Graph loc))
makeGraphB initialGraph =
  pure (pure initialGraph)
