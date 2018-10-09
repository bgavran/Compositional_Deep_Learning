module Autodiff.DType where

import Prelude hiding (id, (.))

import CategoricDefinitions
import Autodiff.GAD
import Autodiff.Dual
import Autodiff.Additive

---------------------------


newtype DType a b = D {
    evalDType :: GADType (DualType (->+)) a b
} deriving (Category, Monoidal, Cartesian, Cocartesian)

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


f :: DType a b -> a -> b
f = fGAD . evalDType

df :: DType a b -> a -> b -> a
df para inp = evalGrad $ (dfGAD . evalDType) para inp


-- compose two parametrized functions
(.<) :: (Monoidal k, _)
    => (q, b) `k` c
    -> (p, a) `k` b
    -> ((p, q), a) `k` c
g .< f = g . (id `x` f) . assocL . (swap `x` id)


---------------------------


p1 :: Num a => DType (a, a) a
p1 = mulC

p2 :: Num a => DType (a, a) a
p2 = addC

p3 :: (Additive a, Num a) => DType ((a, a), a) a
p3 = p2 .< p1 -- composing two parametrized functions

output :: Double
output = f p3 ((3, 5), 7)

derivativeOutput :: ((Double, Double), Double)
derivativeOutput = df p3 ((3, 5), 7) 1
