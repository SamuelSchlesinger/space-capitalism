module Space.Graph where

data Graph l = Graph
  { distance :: l -> l -> Maybe Double } 

mkGraph :: (l -> l -> Bool) -> (l -> l -> Double) -> Graph l
mkGraph connected distance = Graph { distance = \l1 l2 -> if connected l1 l2 then Just (distance l2 l2) else Nothing } 
