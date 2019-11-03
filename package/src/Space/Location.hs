module Space.Location where

import Numeric.Natural
import Space.Graph

data Location =
    Location1
  | Location2
  | Location3 
  | Location4
  | Location5
  deriving (Show, Eq, Enum, Bounded, Ord)

mayTravel
  :: Natural
  -> Location -- ^ Source
  -> Location -- ^ Destination
  -> Graph Location
  -> Bool
mayTravel energy l1 l2 Graph{..} = case distance l1 l2 of
  Just cost -> energy >= cost
  _ -> False
