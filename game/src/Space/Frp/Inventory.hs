module Space.Frp.Inventory where

import Reactive.Banana
import Space.Location
import Space.Graph
import Space.Resource
import Space.Frp.Food
import Space.Frp.Energy

import qualified Data.Map as Map

makeInventoryBB :: MonadMoment m => Event (Location, Location) -> Event () -> Behavior (Graph Location) -> m (Inventory Behavior)
makeInventoryBB travelE tickE graphB = do
  foodB <- makeFoodB tickE
  energyB <- makeEnergyB travelE graphB
  return . Map.fromList $ [(Energy, energyB), (Food, foodB)] <> ((,) <$> tail [Food .. maxBound] <*> repeat 0)
