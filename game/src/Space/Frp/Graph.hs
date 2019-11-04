module Space.Frp.Graph
  ( makeGraphB
  ) where

import Reactive.Banana

import Space.Location
import Space.Graph

makeGraphB :: MonadMoment m => m (Behavior (Graph Location))
makeGraphB =
  pure (pure initialGraph)
 
initialGraph :: Graph Location
initialGraph = Graph \l1 l2 -> if l1 == l2 then Just 0 else Just 1
