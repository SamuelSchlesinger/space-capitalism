module Space.Graph where

import Numeric.Natural

data Graph l = Graph
  { distance :: l -> l -> Maybe Natural }

mkGraph :: (l -> l -> Bool) -> (l -> l -> Natural) -> Graph l
mkGraph connected distance = Graph { distance = \l1 l2 -> if connected l1 l2 then Just (distance l2 l2) else Nothing }
