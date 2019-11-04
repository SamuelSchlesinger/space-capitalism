-- | reactive-banana extras

module Space.Reactive.Banana
  ( leftmostE
  , mapMaybeE
  ) where

import Reactive.Banana

-- | Collapse a list of events to an event that fires with the leftmost event's
-- value (if any).
leftmostE :: [Event a] -> Event a
leftmostE =
  foldr (unionWith const) never

-- | Combination of fmap + filterJust.
mapMaybeE :: (a -> Maybe b) -> Event a -> Event b
mapMaybeE f event =
  filterJust (fmap f event)
