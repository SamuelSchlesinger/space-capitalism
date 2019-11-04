module Space.Resource where

import Numeric.Natural
import Data.Map (Map)
--import qualified Data.Set as Set

data Resource = Energy | Food | Human Skill
  deriving stock (Eq, Ord, Show)

instance Enum Resource where
  fromEnum Energy = 0
  fromEnum Food   = 1
  fromEnum (Human skill) = 2 + fromEnum skill
  toEnum 0 = Energy
  toEnum 1 = Food
  toEnum n = Human (toEnum (n - 2))

instance Bounded Resource where
  maxBound = Human maxBound
  minBound = Energy

data Science = Biology | Sociology | Physics | Computer | Chemistry
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Machine = Power | Weapons | Shields
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Skill = Scientist Science | Machinist Machine
  deriving stock (Eq, Ord, Show)

instance Enum Skill where
  fromEnum (Scientist s) = fromEnum s
  fromEnum (Machinist m) = 5 + fromEnum m 
  toEnum n | n <= 4 = Scientist (toEnum n)
           | otherwise = Machinist (toEnum (n - 5))

instance Bounded Skill where
  minBound = Scientist minBound
  maxBound = Machinist maxBound

type Recipe = Map Resource Natural -> Map Resource Natural

type Inventory f = Map Resource (f Natural)
