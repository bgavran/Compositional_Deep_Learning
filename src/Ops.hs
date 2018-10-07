module Ops where

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P
--import Numeric.LinearAlgebra.Array
--import Numeric.LinearAlgebra.Array.Util

{-
import CategoricDefinitions
import GAD
import Learner

sgd :: (Num p, Fractional p) => p -> p -> p
sgd p pGrad = p - 0.001 * pGrad

dfD :: DType a b -> a -> DType a b
dfD (D op) = snd . op

df :: Additive a => DType a b -> a -> b
df opD v = let dD = dfD opD v
           in f dD one

add :: Additive a => DType (a, a) a
add = jam

addn :: Double -> DType Double Double
addn n = D $ \a -> (a + n, id)

constD :: (Additive a, Num a) => a -> DType a a
constD k = D $ \_ -> (k, scale (zero))

scale :: (Additive a, Num a) => a -> DType a a
scale k = D $ \a -> (a * k, constD k)

mul :: (Additive a, Num a) => DType (a, a) a
mul = D $ \(a, b) -> (a * b, scale b \/ scale a)

expD :: (Additive a, Floating a) => DType a a
expD = D $ \a -> let val = exp a
                 in (val, scale val)

sqr :: (Additive a, Num a) => DType a a
sqr = mul . dup

sqrError :: (Additive a, Num a) => DType (a, a) a
sqrError = sqr . (id \/ scale (-1))

leftToZ :: Additive3 p a b => DType (p, a) b -> DType (Z p, a) b
leftToZ = curryUncurry varToZ

zmul :: (Num a, Additive a) => ParaType a a a
zmul = Para $ leftToZ mul

zadd :: (Num a, Additive a) => ParaType a a a
zadd = Para $ leftToZ add

functorParaD :: ParaType p a b -> DType (Z p, a) b -- need to remove the Z here, it needs to be just a regular tuple?
functorParaD (Para f) = f

functorDPara :: Additive3 p a b => DType (p, a) b -> ParaType p a b
functorDPara = Para . leftToZ

------ This is basically a functor from Para -> Learn. It's defined by a cost and update function.
functorParaLearn :: ParaType p a b -> (p -> p -> p) -> CostF b -> Learner p a b
functorParaLearn para u c = L {
  param = undefined, -- initialized randomly in the shape of param?
  implreq = para,
  upd = \(p, pGrad) -> u <$> p <*> pGrad, -- just applying the update fn recursively to the param data stucture
  cost = c
}

l1 = functorParaLearn zmul sgd trivialCost

l2 = functorParaLearn zadd sgd sqrError

l3 = l2 . l1

l4 = l2 `x` l1

outputLearner l = let p  = param l
                      fl = (f . evalP . implreq) l
                  in (curry fl) p

-- Tensor manipulations

ds # cs = listArray ds cs :: Array Double

sh x = putStr P.. formatFixed 2 $ x -- pretty print the tensor

t1 = [2, 3] # [1, 2..] ! "ij"
t2 = [3, 5] # [1, 1..] ! "jk"

t3 = f mul (t1, t2) -- works out of the box with tensor product (since tensor product is a natural generalization of multiplication)

dt1 = df mul (t1, t2)

{-
sh t1 = [[1, 2, 3],
         [4, 5, 6]]

sh t2 = [[1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1]]

sh t3 = [[6, 6, 6, 6, 6],
         [15, 15, 15, 15, 15],


-}

--
--              
--sigm :: (Cat.Additive a, Floating a) => DType a a
--sigm = D $ \a -> let s = 1 / (1 + exp (-a))
--                 in (s, scale (s * (1 - s)))
--
----D $ \dm -> (dm * s * (1 - s), undefined))
----
----dsigm :: (Floating a, Cat.Additive a) => DType a a
----dsigm = let s = 0.7
----            os = 1 - s
----        in f (Cat.curry mul) s
--
--

-}
