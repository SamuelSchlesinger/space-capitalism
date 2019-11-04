module Space.Frp.Food
  ( makeFoodB
  ) where

import Numeric.Natural
import Reactive.Banana

makeFoodB :: MonadMoment m => Event () -> m (Behavior Natural)
makeFoodB tickE =
  -- Food goes down by one every tick of time.
  accumB
    initialFood
    ((\x -> x - 1) <$ tickE)

initialFood :: Natural
initialFood = 10000
