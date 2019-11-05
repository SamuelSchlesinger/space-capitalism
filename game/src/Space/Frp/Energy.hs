module Space.Frp.Energy
  ( makeEnergyB
  ) where

import Numeric.Natural
import Reactive.Banana

makeEnergyB
  :: MonadMoment m
  => Event (loc, loc, Natural)
  -> m (Behavior Natural)
makeEnergyB travelE =
  accumB
    initialEnergy
    (unions
      [ -- Energy goes down when you travel.
        (\(_source, _destination, distance) energy ->
          energy - distance)
        <$> travelE
      ])

initialEnergy :: Natural
initialEnergy = 100
