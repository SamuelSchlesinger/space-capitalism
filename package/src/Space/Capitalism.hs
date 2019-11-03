module Space.Capitalism
  ( main
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO

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
  { sceneLocation :: Location -- ^ Where the player is.
  , sceneTick :: Int -- ^ Elapsed ticks.
  } deriving stock (Show)

-- | Render a scene to the screen.
render :: Scene -> IO ()
render =
  print

-- | The entire state of the game.
data State
  = State
  { stateLocation :: Location -- ^ Where the player is.
  , stateTick :: Int -- ^ Elapsed ticks.
  }

-- | Cull the game state down to what's relevant for rendering.
stateToScene :: State -> Scene
stateToScene state =
  Scene
    { sceneLocation = stateLocation state
    , sceneTick = stateTick state
    }

-- | Where the player is.
data Location
  = Location1
  | Location2
  | Location3
  | Location4
  | Location5
  deriving stock (Show)

moment :: Event () -> Event Char -> MomentIO (Behavior Scene)
moment tickE charE = do
  tickB :: Behavior Int <-
    accumB 0 ((+1) <$ tickE)

  -- Current location: pressing 1,2,3,4,5 moves to that location immediately.
  locationB :: Behavior Location <-
    stepper
      Location1
      (foldr
        (unionWith const)
        never
        [ Location1 <$ filterE (== '1') charE
        , Location2 <$ filterE (== '2') charE
        , Location3 <$ filterE (== '3') charE
        , Location4 <$ filterE (== '4') charE
        , Location5 <$ filterE (== '5') charE
        ])

  let
    stateB :: Behavior State
    stateB =
      State
        <$> locationB
        <*> tickB

  let
    sceneB :: Behavior Scene
    sceneB =
      stateToScene <$> stateB

  pure sceneB
