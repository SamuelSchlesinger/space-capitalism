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

data MayTravelResult
  -- | Source is not connected to destination.
  = MayTravelResult'NotConnected
  -- | Not enough energy to travel.
  | MayTravelResult'NotEnoughEnergy
  -- | Source *is* destination.
  | MayTravelResult'SourceIsDestination
  -- | OK!
  | MayTravelResult'Success

mayTravel
  :: Natural
  -> loc -- ^ Source
  -> loc -- ^ Destination
  -> Graph loc
  -> MayTravelResult
mayTravel energy l1 l2 Graph{..} =
  case distance l1 l2 of
    Just 0 -> MayTravelResult'SourceIsDestination
    Just cost ->
      if fromIntegral energy >= cost then
        MayTravelResult'Success
      else
        MayTravelResult'NotEnoughEnergy
    _ -> MayTravelResult'NotConnected
