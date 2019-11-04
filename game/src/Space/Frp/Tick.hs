module Space.Frp.Tick
  ( makeTickB
  ) where

import Reactive.Banana

makeTickB :: MonadMoment m => Event () -> m (Behavior Int)
makeTickB tickE =
  accumB 0 ((+1) <$ tickE)
