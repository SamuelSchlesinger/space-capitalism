-- | reactive-banana extras

module Space.Reactive.Banana
  ( leftmostE
  ) where

import Reactive.Banana

-- | Collapse a list of events to an event that fires with the leftmost event's
-- value (if any).
leftmostE :: [Event a] -> Event a
leftmostE =
  foldr (unionWith const) never
