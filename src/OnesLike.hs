module OnesLike where

import CategoricDefinitions
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

-- this is all done at runtime, unfortunately
class OnesLike a where
    onesLike :: a -> a

instance OnesLike () where
    onesLike _ = ()

instance {-# OVERLAPPABLE #-} Num a => OnesLike a where
    onesLike _ = 1

instance {-# OVERLAPPING #-} OnesLike Tensor where
    -- Assumes single-letter index names
    onesLike c = let d  = map iDim $ dims c
                     ch = concatMap iName $ dims c
                 in d # repeat 1 ! ch

ds # cs = listArray ds cs :: Array Double
