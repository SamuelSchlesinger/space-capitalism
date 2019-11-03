module Space.Graph where

import Numeric.Natural

data Graph l = Graph
  { distance :: l -> l -> Maybe Natural } 
