module Space.HKD
  ( HKD
  ) where

import Data.Functor.Identity

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a
