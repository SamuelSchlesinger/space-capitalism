module Space.Scene where

import Space.State
import Space.Resource
import Space.Location

data Scene = Scene
  { sceneInventory :: Inventory
  , sceneLocation :: Location
  , sceneTick :: Int 
  } deriving stock (Show)

render :: Scene -> IO ()
render = print

stateToScene :: State -> Scene
stateToScene (State inv loc t) = Scene inv loc t
