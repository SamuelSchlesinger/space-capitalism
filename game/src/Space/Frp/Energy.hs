module Space.Frp.Energy
  ( makeEnergyB
  ) where

import Data.Maybe
import Numeric.Natural
import Reactive.Banana

import Space.Graph (Graph(..))

makeEnergyB
  :: MonadMoment m
  => Event (loc, loc)
  -> Behavior (Graph loc)
  -> m (Behavior Natural)
makeEnergyB travelE graphB =
  accumB
    initialEnergy
    (unions
      [ -- Energy goes down when you travel.
        (\Graph{..} (source, destination) energy ->
          energy - ceiling (fromJust (distance source destination)))
        <$> graphB <@> travelE
      ])

initialEnergy :: Natural
initialEnergy = 100
