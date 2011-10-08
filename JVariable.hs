module JVariable where

import Data.Map (Map)

data JVariable 
  = VInteger Int
  | VStaticField String String
  | VString String
  | VBoolean Bool
  | VObjectRef Int -- ^ the object is on the heap
  | VObject (Map String JVariable) -- String should be replaced
  | VNull
 deriving (Show, Eq)

instance Ord JVariable where
    compare v1 v2 = case (v1,v2) of
        (VBoolean v1, VBoolean v2) -> v1 `compare` v2
        (VInteger v1, VInteger v2) -> v1 `compare` v2

instance Num JVariable where
    (+) v1 v2 = case (v1,v2) of
        (VInteger v1, VInteger v2) -> VInteger $ v1 + v2
    (*) v1 v2 = case (v1,v2) of
        (VInteger v1, VInteger v2) -> VInteger $ v1 * v2
    negate (VInteger v1) = (VInteger $ -v1)
    abs (VInteger v) = VInteger $ abs v
    signum (VInteger v) = VInteger $ signum v
