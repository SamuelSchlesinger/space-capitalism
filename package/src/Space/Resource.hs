module Space.Resource where

import Numeric.Natural
import Data.Map (Map)
--import qualified Data.Set as Set

data Resource = Energy | Food | Human Skill
  deriving stock (Eq, Ord, Show)

data Science = Biology | Sociology | Physics | Computer | Chemistry
  deriving stock (Eq, Ord, Show)

data Machine = Power | Weapons | Shields
  deriving stock (Eq, Ord, Show)

data Skill = Scientist Science | Machinist Machine
  deriving stock (Eq, Ord, Show)

type Recipe = Map Resource Natural -> Map Resource Natural

type Inventory = Map Resource Natural
