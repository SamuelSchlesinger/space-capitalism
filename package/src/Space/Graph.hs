module Space.Graph where

import Data.Set (Set)
import qualified Data.Set as Set

newtype Graph l = Graph
  { unGraph :: l -> Set l }

adjacent :: Ord l => Graph l -> l -> l -> Bool
adjacent (Graph g) l1 l2 = l2 `Set.member` g l1 && l1 `Set.member` g l2 
