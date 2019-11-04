module Space.Location where

import Numeric.Natural
import Space.Graph

data Location =
    Xarkov
  | Kalinpol
  | Partook
  | Yenlo
  | Karkharov
  deriving (Show, Eq, Enum, Bounded, Ord)

initialLocation :: Location
initialLocation =
  Kalinpol

mayTravel
  :: Natural
  -> loc -- ^ Source
  -> loc -- ^ Destination
  -> Graph loc
  -> Bool
mayTravel energy l1 l2 Graph{..} = case distance l1 l2 of
  Just cost -> fromIntegral energy >= cost
  _ -> False
