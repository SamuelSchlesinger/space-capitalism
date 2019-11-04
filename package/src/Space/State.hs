module Space.State where

import Space.Resource
import Space.Location

data State = State
  { stateInventory :: Inventory
  , stateLocation :: Location
  , stateTick :: Int }
