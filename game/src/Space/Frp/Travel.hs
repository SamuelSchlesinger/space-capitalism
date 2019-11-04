module Space.Frp.Travel
  ( makeTravelE
  ) where

import Control.Monad
import Numeric.Natural
import Reactive.Banana

import Space.Graph (Graph)
import Space.Location (mayTravel)
import Space.Reactive.Banana

-- Travel event: emits a (source, destination) pair every time the player
-- successfully travels from source to destination.
makeTravelE
  :: (Bounded loc, Enum loc)
  => Event Char
  -> Behavior Natural -- ^ Energy.
  -> Behavior (Graph loc) -- ^ Graph.
  -> Behavior loc -- ^ Location.
  -> Event (loc, loc)
makeTravelE charE energyB graphB locationB =
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
        ([ minBound .. maxBound ] `zip` take 9 ['1' .. ]))
