module Autodiff.D where

import Prelude hiding (id, (.))
import Control.Lens hiding ((#), para)
import Control.Arrow

import CategoricDefinitions
import Autodiff.GAD
import Autodiff.Dual
import Autodiff.Additive
import OnesLike

---------------------------


newtype DType a b = D {
    _evalDType :: GADType (DualType (->+)) a b
} deriving (Category, Monoidal, Cocartesian)

makeLenses ''DType


{-
Manually deriving the Cartesian instance because we need to manually derive counit.
We need to manually derive counit because the only way to propagate
tensor shape to the gradient computation is during runtime (see onesLike function definition).
This is because we don't have statically known tensor dimensions
-}
instance Cartesian DType where
    type AllowedCar DType a = (AllowedCar (GADType (DualType (->+))) a, OnesLike a)
    exl = D exl
    exr = D exr
    dup = D dup
    counit = D $ GAD $ \a -> ((), Dual $ AddFun $ const (onesLike a))

instance Num s => NumCat DType s where
    negateC = D negateC
    addC = D addC
    mulC = D mulC
    increaseC a = D $ increaseC a

instance Floating s => FloatCat DType s where
    expC = D expC

instance Fractional s => FractCat DType s where
    recipC = D recipC

---------------------------

grad' :: DType a b -> a -> b -> a
grad' d a = (d ^. evalDType.evalGAD) a ^. (_2.evalDual.evalAF)

-- just normal output of DType
f :: DType a b -> a -> b
f d a = (d ^. evalDType.evalGAD) a ^. _1

-- grad is grad' with ones as the incoming derivative
grad :: _ => DType a b -> a -> a
grad dt' t  = grad' (counit . dt') t ()

partiallyApply :: AllAdditive [a, b, c] => DType (a, b) c -> a -> DType b c
partiallyApply dt a = D $ GAD $ \b -> second (.inr) $ (dt ^. evalDType.evalGAD) (a, b)


---------------------------

p1 :: Num a => DType (a, a) a
p1 = mulC

p2 :: Num a => DType (a, a) a
p2 = addC

p3 :: (OnesLike a, Additive a, Num a) => DType ((a, a), a) a
p3 = p2 .-- p1 -- composing two 'parametrized' functions

output :: Double
output = f p3 ((3, 5), 7)

derivativeOutput :: ((Double, Double), Double)
derivativeOutput = grad p3 ((3, 5), 7)
