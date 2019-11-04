module Space.State where

import Data.Functor.Identity
import Space.Resource
import Space.Location

data State = State
  { stateInventory :: Inventory Identity
  , stateLocation :: Location
  , stateTick :: Int }
