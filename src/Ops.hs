module Ops where

import Prelude hiding ((.), id)
import Autodiff.DType
import Autodiff.GAD
import Autodiff.Additive
import Autodiff.Dual
import CategoricDefinitions

sigmoid :: (Additive a, Floating a) => DType a a
sigmoid = recipC . increaseC 1 . expC . negateC

relu :: DType Double Double
relu = D $ GAD $ \a -> let b = if a < 0 then 0 else 1
                       in (b*a, Dual $ AddFun (*b))

sgd :: Fractional p => (p, p) -> p
sgd (p, pGrad) = p - 0.1 * pGrad

sqDiff :: DType (Double, Double) Double
sqDiff = mulC . dup . (id \/ negateC)


{-
   e^a + a * b
    |
  /  \      jam
 |    |
 |   / \   exp `x` mulC
 |  (|  |)  assocL
(|  |)  |
( \/ )  |   dup `x` id
(  | )  |
   a    b
-}
myf :: (Additive a, Floating a) => DType (a, a) a
myf = jam . (expC `x` mulC) . assocL . (dup `x` id)
