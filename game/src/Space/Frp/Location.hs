module Space.Frp.Location
  ( makeLocationEB
  ) where

import Reactive.Banana

-- Player location.
makeLocationEB
  :: forall loc m.
     MonadMoment m
  => loc -- ^ Initial location.
  -> Event (loc, loc) -- ^ Travel event.
  -> m (Event loc, Behavior loc)
makeLocationEB initialLocation travelE =
  (locationE ,) <$>
    stepper initialLocation locationE

  where
    locationE :: Event loc
    locationE =
      snd <$> travelE
