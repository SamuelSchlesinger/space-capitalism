module Space.Capitalism
  ( main
  ) where

import Data.Maybe
import Control.Concurrent
import Control.Exception
import Control.Monad
import Numeric.Natural
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO
import qualified Data.Map as Map

import Space.State
import Space.Scene
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

  -- Handle user input forever
  hSetBuffering stdin NoBuffering
  bracket_
    (hSetEcho stdin False)
    (hSetEcho stdin True)
    (forever (getChar >>= fireChar))

moment :: Event () -> Event Char -> MomentIO (Behavior Scene)
moment tickE charE = mdo
  tickB :: Behavior Int <-
    accumB 0 ((+1) <$ tickE)

  graphB :: Behavior (Graph Location) <-
    pure (pure initialGraph)

  -- Energy.
  energyB :: Behavior Natural <-
    accumB
      initialEnergy
      (unions
        [ -- Energy goes down when you travel.
          (\Graph{..} (source, destination) energy ->
            energy - ceiling (fromJust (distance source destination)))
          <$> graphB <@> travelE
        ])

  -- Food goes down by one every tick of time.
  foodB :: Behavior Natural <-
    accumB
      1000
      ((\x -> x - 1) <$ tickE)
   -- pure (pure 100)

  -- The inventory map behavior is a boilerplate applicative combination of the
  -- individual resource behaviors
  let
    inventoryB :: Behavior Inventory
    inventoryB =
      Map.fromList <$>
        traverse
          (\(resource, amountB) ->
            (resource ,) <$> amountB)
          [ (Energy, energyB)
          , (Food, foodB)
          ]

  -- Player location event: emits a location every time the player travels to
  -- it.
  let
    locationE :: Event Location
    locationE =
      snd <$> travelE
  locationB :: Behavior Location <-
    stepper initialLocation locationE

  -- Travel event: emits a (source, destination) pair every time the player
  -- successfully travels from source to destination.
  let
    travelE :: Event (Location, Location)
    travelE =
      leftmostE
        (map
          (\(destination, char) ->
            filterJust
              ((\location energy graph -> do
                guard (mayTravel energy location destination graph)
                pure (location, destination))
                <$> locationB
                <*> energyB
                <*> graphB
                <@  filterE (== char) charE))
            ([ minBound .. maxBound ] `zip` ['1' .. ]))

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

initialEnergy :: Natural
initialEnergy =
  100

initialGraph :: Graph Location
initialGraph = Graph 
  { distance = \l1 l2 -> if l1 == l2 then Just 0 else Just 1 }

-- | Collapse a list of events to an event that fires with the leftmost event's
-- value (if any).
leftmostE :: [Event a] -> Event a
leftmostE =
  foldr (unionWith const) never
