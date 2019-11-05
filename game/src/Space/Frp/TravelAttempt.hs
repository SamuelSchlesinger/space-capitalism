module Space.Frp.TravelAttempt
  ( TravelAttempt(..)
  , makeTravelAttemptE
  ) where

import Numeric.Natural
import Reactive.Banana

import Space.Graph (Graph)
import Space.Location (MayTravelResult(..), mayTravel)
import Space.Reactive.Banana

data TravelAttempt loc
  -- | Not enough energy to travel.
  = TravelAttempt'NotEnoughEnergy
  -- | Travel attempt from source to destination successful.
  | TravelAttempt'Success loc loc Natural
  deriving stock (Eq)

-- Travel attempt event: emits when the player tries to travel (but not when
-- they try to travel to they planet they are already on).
makeTravelAttemptE
  :: forall loc.
     (Bounded loc, Enum loc)
  => Event Char
  -> Behavior Natural -- ^ Energy.
  -> Behavior (Graph loc) -- ^ Graph.
  -> Behavior loc -- ^ Location.
  -> Event (TravelAttempt loc)
makeTravelAttemptE charE energyB graphB locationB =
  leftmostE (map makeLocKeyE locKeys)

  where
    locKeys :: [(loc, Char)]
    locKeys =
      [ minBound .. maxBound ] `zip` take 9 ['1' .. ]

    makeLocKeyE :: (loc, Char) -> Event (TravelAttempt loc)
    makeLocKeyE (destination, char) =
      filterJust
        (attemptTravel
          <$> locationB
          <*> pure destination
          <*> energyB
          <*> graphB
          <@  filterE (== char) charE)

attemptTravel
  :: loc -- ^ Source.
  -> loc -- ^ Destination.
  -> Natural -- ^ Energy.
  -> Graph loc -- ^ Graph.
  -> Maybe (TravelAttempt loc)
attemptTravel location destination energy graph =
  case mayTravel energy location destination graph of
    MayTravelResult'NotConnected -> Nothing
    MayTravelResult'NotEnoughEnergy ->
      Just TravelAttempt'NotEnoughEnergy
    MayTravelResult'SourceIsDestination -> Nothing
    MayTravelResult'Success distance ->
      Just (TravelAttempt'Success location destination distance)
