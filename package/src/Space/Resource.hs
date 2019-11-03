module Space.Resource where

import Numeric.Natural
import Data.Map (Map)
import Data.Set (Set)
--import qualified Data.Set as Set

data Resource = Energy | Food | Human (Set Skill)

data Science = Biology | Sociology | Physics | Computer | Chemistry

data Machine = Power | Weapons | Shields

data Skill = Scientist (Set Science) | Machinist (Set Machine) 

type Recipe = Map Resource Natural -> Map Resource Natural

type Inventory = Map Resource Natural
