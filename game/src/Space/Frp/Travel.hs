module Space.Frp.Travel
  ( makeTravelE
  ) where

import Numeric.Natural
import Reactive.Banana

import Space.Frp.TravelAttempt (TravelAttempt(..))
import Space.Reactive.Banana

makeTravelE
  :: Event (TravelAttempt loc)
  -> Event (loc, loc, Natural)
makeTravelE travelAttemptE =
  mapMaybeE
    (\case
      TravelAttempt'NotEnoughEnergy -> Nothing
      TravelAttempt'Success source destination distance ->
        Just (source, destination, distance))
    travelAttemptE
