module Space.Capitalism
  ( main
  ) where

import Control.Concurrent
import Control.Monad
import Reactive.Banana
import Reactive.Banana.Frameworks

main :: IO ()
main = do
  (tickAddHandler, fireTick) <- newAddHandler

  (void . forkIO . forever) do
    fireTick ()
    threadDelay 1_000_000

  network <-
    compile do
      -- Boilerplate!
      tickE <- fromAddHandler tickAddHandler
      sceneB <- moment tickE
      initialScene <- valueB sceneB
      liftIO (render initialScene)
      sceneE <- changes sceneB
      reactimate' (fmap (fmap render) sceneE)

  actuate network

  forever (threadDelay maxBound)

type Scene = Int

render :: Scene -> IO ()
render n =
  print n

moment :: Event () -> MomentIO (Behavior Scene)
moment tickE = do
  sceneB <- accumB 0 ((+1) <$ tickE)
  pure sceneB
