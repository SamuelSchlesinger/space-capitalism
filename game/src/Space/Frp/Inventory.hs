module Space.Frp.Inventory where

import Data.Functor.Identity
import Numeric.Natural
import Reactive.Banana
import qualified Data.Map as Map

import Space.Location
import Space.Resource
import Space.Frp.Food
import Space.Frp.Energy

makeInventoryBB
  :: MonadMoment m
  => Event (Location, Location, Natural)
  -> Event ()
  -> m (Inventory Behavior, Behavior (Inventory Identity))
makeInventoryBB travelE tickE = do
  foodB <- makeFoodB tickE
  energyB <- makeEnergyB travelE
  let inventoryBB =  Map.fromList $ [(Energy, energyB), (Food, foodB)] <> ((,) <$> tail [Food .. maxBound] <*> [0])
  let inventoryB = Map.foldrWithKey (\res b m -> Map.insert res <$> b <*> m) (pure Map.empty) inventoryBB
  return (inventoryBB, inventoryB)
