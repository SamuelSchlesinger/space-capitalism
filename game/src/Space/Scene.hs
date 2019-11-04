module Space.Scene where

import Data.Functor.Identity
import Space.State
import Space.Resource
import Space.Location

data Scene = Scene
  { sceneInventory :: Inventory Identity
  , sceneLocation :: Location
  , sceneTick :: Int 
  } deriving stock (Show)

render :: Scene -> IO ()
render = print

stateToScene :: State -> Scene
stateToScene (State inv loc t) = Scene inv loc t
