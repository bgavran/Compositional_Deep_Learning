module Autodiff.GAD where

import Prelude hiding (id, (.))
import Data.Kind (Type)
import Control.Lens hiding ((#), para)

import CategoricDefinitions
import Autodiff.Additive
import Autodiff.Cont
import Autodiff.Dual

newtype GADType (k :: Type -> Type -> Type) a b = GAD {
    _evalGAD :: a -> (b, a `k` b)
}

makeLenses ''GADType

linearD :: (a -> b) -> (a `k` b) -> GADType k a b
linearD f f' = GAD $ \x -> (f x, f')

instance Category k => Category (GADType k) where
    type Allowed (GADType k) a = (Additive a, Allowed k a)

    id = linearD id id
    GAD g . GAD f   = GAD $ \a -> let (b, f') = f a
                                      (c, g') = g b
                                  in (c, g' . f')

instance Monoidal k => Monoidal (GADType k) where
    GAD f `x` GAD g = GAD $ \(a, b) -> let (c, f') = f a
                                           (d, g') = g b
                                       in ((c, d), f' `x` g')
    assocL = linearD assocL assocL
    assocR = linearD assocR assocR
    unitorL = linearD unitorL unitorL
    unitorL' = linearD unitorL' unitorL'
    swap = linearD swap swap

instance Cartesian k => Cartesian (GADType k) where
    type AllowedCar (GADType k) a = AllowedCar k a

    exl = linearD exl exl
    exr = linearD exr exr
    dup = linearD dup dup
    counit = linearD counit counit

instance Cocartesian k => Cocartesian (GADType k) where
    type AllowedCoCar (GADType k) a = (AllowedCoCar k a, Allowed (GADType k) a) -- whatever the category k allows + additive

    inl = linearD inlF inl
    inr = linearD inrF inr
    jam = linearD jamF jam
    unit = linearD unitF unit

-- Can the set of these constraints be shortened?
-- negateC and addC differ from paper; 2nd argument to linearD is changed so the constraint NumCat k s isn't needed anymore
instance (Num s, Scalable k s, Monoidal k, Cocartesian k,
          Allowed k (s, s), Allowed k s, AllowedCoCar k s) => NumCat (GADType k) s where
    negateC = linearD negateC (scale (-1)) -- this is where this differs from SimpleAD paper
    addC = linearD addC jam
    mulC = GAD $ \(a, b) -> (a * b, scale b \/ scale a) -- most of the instance constraints come from \/
    increaseC a = linearD (increaseC a) id

instance (Floating s, FloatCat k s, Scalable k s) => FloatCat (GADType k) s where
    expC = GAD $ \a -> let e = exp a
                       in (e, scale e)

instance (Fractional s, FractCat k s, Scalable k s) => FractCat (GADType k) s where
    recipC = GAD $ \a -> let r = recip a
                         in (r, scale (-r*r))
