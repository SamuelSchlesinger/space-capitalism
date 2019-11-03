module Space.Resource where

import Numeric.Natural
import Data.Map (Map)
--import qualified Data.Set as Set

data Resource = Energy | Food | Human Skill

data Science = Biology | Sociology | Physics | Computer | Chemistry

data Machine = Power | Weapons | Shields

data Skill = Scientist Science | Machinist Machine 

type Recipe = Map Resource Natural -> Map Resource Natural

type Inventory = Map Resource Natural
