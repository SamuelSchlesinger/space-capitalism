module Space.Frp.Travel
  ( makeTravelE
  ) where

import Reactive.Banana

import Space.Frp.TravelAttempt (TravelAttempt(..))
import Space.Reactive.Banana

makeTravelE
  :: Event (TravelAttempt loc)
  -> Event (loc, loc)
makeTravelE travelAttemptE =
  mapMaybeE
    (\case
      TravelAttempt'NotEnoughEnergy -> Nothing
      TravelAttempt'Success source destination ->
        Just (source, destination))
    travelAttemptE
