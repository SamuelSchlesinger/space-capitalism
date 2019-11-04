module Space.Market where

import Data.Functor.Const
import Data.Functor.Compose
import Space.Resource
import Numeric.Natural

data Market f = Market {
    supply :: Inventory f
  , sellPrice :: Inventory (Compose f (Const Natural))
  , buyPrice :: Inventory (Compose f (Const Natural))
}
