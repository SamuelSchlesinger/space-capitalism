module Space.Frp.Food
  ( makeFoodB
  ) where

import Numeric.Natural
import Reactive.Banana

makeFoodB :: MonadMoment m => Natural -> Event () -> m (Behavior Natural)
makeFoodB initialFood tickE =
  -- Food goes down by one every tick of time.
  accumB
    initialFood
    ((\x -> x - 1) <$ tickE)
