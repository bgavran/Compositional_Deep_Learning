module Autodiff.Dual where

import Prelude hiding (id, (.), curry, uncurry)
import Control.Lens hiding ((#), para)

import qualified Prelude as P

import CategoricDefinitions

newtype DualType k a b = Dual {
    _evalDual :: b `k` a
}
makeLenses ''DualType

instance Category k => Category (DualType k) where
    type Allowed (DualType k) a = Allowed k a

    id = Dual id
    Dual g . Dual f = Dual (f . g)

instance Monoidal k => Monoidal (DualType k) where
    Dual f `x` Dual g = Dual (f `x` g)
    assocL = Dual assocR
    assocR = Dual assocL
    --unitorL = Dual unitorL'
    --unitorL' = Dual unitorL
    --unitorR = Dual unitorR'
    --unitorR' = Dual unitorR
    swap = Dual swap

instance (Cartesian k, Cocartesian k) => Cartesian (DualType k) where
    type AllowedCar (DualType k) a = AllowedCoCar k a

    exl = Dual inl
    exr = Dual inr
    dup = Dual jam
    counit = Dual unit

instance Cartesian k => Cocartesian (DualType k) where
    type AllowedCoCar (DualType k) a = AllowedCar k a

    inl = Dual exl
    inr = Dual exr
    jam = Dual dup
    unit = Dual counit

instance Scalable k a => Scalable (DualType k) a where
    scale s = Dual (scale s) -- is this okay? maybe scale 1/s?

instance FloatCat k s => FloatCat (DualType k) s where
    expC = Dual expC

instance FractCat k s => FractCat (DualType k) s where
    recipC = Dual recipC

