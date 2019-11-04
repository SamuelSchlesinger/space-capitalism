module Space.Capitalism
  ( main
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Numeric.Natural
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO
import qualified Data.Map as Map

import Space.Frp
import Space.Graph
import Space.Location
import Space.Resource
import Space.Scene
import Space.State

main :: IO ()
main = do
  (charAddHandler, fireChar) <- newAddHandler
  (tickAddHandler, fireTick) <- newAddHandler

  -- TODO: initialize a number of handlers here which will be events to
  -- affect the various markets in the system. fire these events in some
  -- clever way that makes the player feel like the arbitrage opportunities
  -- keep disappearing on them, while generating new ones. ideally, as the
  -- game goes on, the economy should get "tighter", meaning that arbitrage
  -- opportunities are hard to come by.
  --
  -- what is an arbitrage opportunity? it is basically just a way that you
  -- can buy and sell goods in order to make a profit. if you can buy good
  -- A on planet X for 1 unit each and sell good A on planet Y for 2 units
  -- each, then as long as the transportation costs are accounted for in
  -- your profits you can likely make a profit off running good A from
  -- planet X to planet Y. the game should detect that good A is being
  -- bought a lot and increase the demand for good A, meaning that its
  -- price should slowly rise to make this arbitrage opportunity less and
  -- less lucrative. similarly, planet Y should have the supply increase
  -- and thus the price decrease, coming at you from both angles.

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
  tickB <- makeTickB tickE
  graphB <- makeGraphB initialGraph
  energyB <- makeEnergyB initialEnergy travelE graphB
  foodB <- makeFoodB initialFood tickE
  (_locationE, locationB) <- makeLocationEB initialLocation travelE
  let travelE = makeTravelE charE energyB graphB locationB

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

initialFood :: Natural
initialFood =
  100

initialGraph :: Graph Location
initialGraph = Graph
  { distance = \l1 l2 -> if l1 == l2 then Just 0 else Just 1 }
