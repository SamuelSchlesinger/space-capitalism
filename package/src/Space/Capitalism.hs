module Space.Capitalism
  ( main
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO
import qualified Data.Map as Map

import Space.Graph
import Space.Resource
import Space.Location

main :: IO ()
main = do
  (charAddHandler, fireChar) <- newAddHandler
  (tickAddHandler, fireTick) <- newAddHandler

  (void . forkIO . forever) do
    fireTick ()
    threadDelay 1_000_000

  actuate =<<
    compile do
      -- Define the game and extract a time-varying scene to render
      charE <- fromAddHandler charAddHandler
      tickE <- fromAddHandler tickAddHandler
      sceneB <- moment tickE charE
      -- Render the initial scene...
      initialScene <- valueB sceneB
      liftIO (render initialScene)
      -- ...and every scene thereafter
      sceneE <- changes sceneB
      reactimate' (fmap (fmap render) sceneE)

  -- Handle user-input (there isn't any yet, so we just sleep forever).
  hSetBuffering stdin NoBuffering
  bracket_
    (hSetEcho stdin False)
    (hSetEcho stdin True)
    (forever do
      getChar >>= fireChar)

-- | The information relevant for rendering
data Scene
  = Scene
  { sceneInventory :: Inventory
  , sceneLocation :: Location -- ^ Where the player is.
  , sceneTick :: Int -- ^ Elapsed ticks.
  } deriving stock (Show)

-- | Render a scene to the screen.
render :: Scene -> IO ()
render =
  print

-- | The entire state of the game.
data State
  = State
  { stateInventory :: Inventory
  , stateLocation :: Location -- ^ Where the player is.
  , stateTick :: Int -- ^ Elapsed ticks.
  }

-- | Cull the game state down to what's relevant for rendering.
stateToScene :: State -> Scene
stateToScene state =
  Scene
    { sceneInventory = stateInventory state
    , sceneLocation = stateLocation state
    , sceneTick = stateTick state
    }

moment :: Event () -> Event Char -> MomentIO (Behavior Scene)
moment tickE charE = mdo
  tickB :: Behavior Int <-
    accumB 0 ((+1) <$ tickE)

  graphB :: Behavior (Graph Location) <-
    pure (pure initialGraph)

  -- Player inventory.
  inventoryB :: Behavior Inventory <-
    accumB
      initialInventory
      (unions
        [ undefined travelE
        ])

  -- Current location: pressing 1,2,3,4,5 moves to that location immediately.
  let
    locationE :: Event Location
    locationE =
      snd <$> travelE
  locationB :: Behavior Location <-
    stepper initialLocation locationE

  let
    travelE :: Event (Location, Location)
    travelE =
      leftmostE
        (map
          (\(destination, char) ->
            filterJust
              ((\location inventory graph -> do
                guard (mayTravel inventory location destination graph)
                pure (location, destination))
                <$> locationB
                <*> inventoryB
                <*> graphB
                <@  filterE (== char) charE))
          [ (Location1, '1')
          , (Location2, '2')
          , (Location3, '3')
          , (Location4, '4')
          , (Location5, '5')
          ])

  let
    stateB :: Behavior State
    stateB =
      State
        <$> inventoryB
        <*> locationB
        <*> tickB

  let
    sceneB :: Behavior Scene
    sceneB =
      stateToScene <$> stateB

  pure sceneB

initialInventory :: Inventory
initialInventory =
  Map.fromList
    [ (Energy, 100)
    , (Food, 100)
    ]

initialLocation :: Location
initialLocation =
  Location1

initialGraph :: Graph Location
initialGraph = Graph 
  { distance = \l1 l2 -> if l1 == l2 then Just 0 else Just 1 }

-- | Collapse a list of events to an event that fires with the leftmost event's
-- value (if any).
leftmostE :: [Event a] -> Event a
leftmostE =
  foldr (unionWith const) never
