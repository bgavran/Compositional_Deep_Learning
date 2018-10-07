module Train where

import Prelude hiding (id, (.))
--import Numeric.LinearAlgebra.Array
--import Numeric.LinearAlgebra.Array.Util

import CategoricDefinitions
import GAD
import Para

fp :: ParaType p a b -> (PType p, a) -> b
fp = fGAD . evalDType . evalPara

dfp :: ParaType p a b -> (PType p, a) -> b -> (PType p, a)
dfp para inp = evalGrad $ (dfGAD . evalDType . evalPara) para inp

f :: DType a b -> a -> b
f = fGAD . evalDType

df :: DType a b -> a -> b -> a
df para inp = evalGrad $ (dfGAD . evalDType) para inp

myf :: (Additive a, Floating a) => DType (a, a) a
myf = jam . (expC `x` mulC) . assocL . (dup `x` id)


-------------------------------------------

sigmoid :: (Additive a, Floating a) => DType a a
sigmoid = recipC . increaseC 1 . expC . negateC

p1 :: (Additive a, Num a) => ParaType a a a
p1 = Para (mulC . leftFromP)

p2 :: (Additive a, Num a) => ParaType a a a
p2 = Para (addC . leftFromP)

p3 :: (Additive a, Num a) => ParaType a a a
p3 = p2 . p1 -- composing two parametrized functions

output :: Double
output = fp p3 (P 2 `X` P 3, 7)

derivativeOutput :: (PType Double, Double)
derivativeOutput = dfp p3 (P 2 `X` P 3, 7) 1
