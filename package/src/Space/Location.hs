module Space.Location where

import Space.Graph
import Space.Resource

data Location =
    Location1
  | Location2
  | Location3 
  | Location4
  | Location5
  deriving (Show, Eq, Enum, Bounded, Ord)

mayTravel
  :: Inventory
  -> Location -- ^ Source
  -> Location -- ^ Destination
  -> Graph Location
  -> Bool
mayTravel _ _ _ _ =
  True
