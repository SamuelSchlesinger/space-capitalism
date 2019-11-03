module Space.Resource where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
--import qualified Data.Set as Set

data Resource = Energy | Food | Human (Set Skill)

data Science = Biology | Sociology | Physics | Computer | Chemistry

data Machine = Power | Weapons | Shields

data Skill = Scientist (Set Science) | Machinist (Set Machine) 

type Recipe = (Map Resource Integer, Map Resource Integer)

multiplyRecipe :: Integer -> Recipe -> Recipe
multiplyRecipe m (from, to) = (Map.map (* m) from, Map.map (* m) to) 

type Inventory = Map Resource Integer
