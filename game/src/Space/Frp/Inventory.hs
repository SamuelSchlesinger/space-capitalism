module Space.Frp.Inventory where

import Reactive.Banana
import Space.Location
import Space.Graph
import Space.Resource
import Space.Frp.Food
import Space.Frp.Energy

import Data.Functor.Identity
import qualified Data.Map as Map

makeInventoryBB
  :: MonadMoment m
  => Event (Location, Location)
  -> Event ()
  -> Behavior (Graph Location)
  -> m (Inventory Behavior, Behavior (Inventory Identity))
makeInventoryBB travelE tickE graphB = do
  foodB <- makeFoodB tickE
  energyB <- makeEnergyB travelE graphB
  let inventoryBB =  Map.fromList $ [(Energy, energyB), (Food, foodB)] <> ((,) <$> tail [Food .. maxBound] <*> [0])
  let inventoryB = Map.foldrWithKey (\res b m -> Map.insert res <$> b <*> m) (pure Map.empty) inventoryBB
  return (inventoryBB, inventoryB)
