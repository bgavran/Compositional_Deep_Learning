{-# LANGUAGE 
             EmptyCase,
             FlexibleInstances,
             FlexibleContexts,
             InstanceSigs,
             MultiParamTypeClasses,
             PartialTypeSignatures,
             LambdaCase,
             MultiWayIf,
             NamedFieldPuns,
             TupleSections,
             DeriveFunctor,
             TypeOperators,
             ScopedTypeVariables,
             ConstraintKinds,
             RankNTypes,
             NoMonomorphismRestriction,
             TypeFamilies,
             UndecidableInstances,
             GeneralizedNewtypeDeriving
                            #-}

module AD where

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

import CategoricDefinitions
import Additive
import Dual
import Cont

newtype GADType k a b = GAD {
  evalGAD :: a -> (b, a `k` b) 
}

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

instance Cartesian k => Cartesian (GADType k) where
  type AllowedCar (GADType k) a = AllowedCar k a

  exl = linearD exl exl
  exr = linearD exr exr
  dup = linearD dup dup

instance Cocartesian k => Cocartesian (GADType k) where
  type AllowedCoCar (GADType k) a = (AllowedCoCar k a, Allowed (GADType k) a) -- whatever the category k allows + additive

  inl = linearD inlF inl
  inr = linearD inrF inr
  jam = linearD jamF jam

instance (Num s, NumCat k s, Scalable k s) => NumCat (GADType k) s where
  negateC = linearD negateC negateC
  addC = linearD addC addC
  mulC = undefined
  --mulC = GAD $ \(a, b) -> (a * b, scale b \/ scale a)

mul :: (Additive a, Num a) => BackpropType (a, a) a
mul = GAD $ \(a, b) -> (a * b, scale b \/ scale a)
------------------------------------------------------------------------

{-
Notes:
It seems the newtype D k a b needs to be recursively defined if D is to be a CCC.
Right now stuff seems to be missing and preventing the operations to be..., well closed.

-}

--newtype GD a b = GD (GADType (DualType (->+)) a b)
--  deriving (Category, Monoidal, Cartesian, Cocartesian)

newtype DType a b = D (BackpropType a b) 
  deriving (Category, Monoidal, Cartesian, Cocartesian)

type BackpropType a b = GADType (DualType (->+)) a b

evalD :: DType a b -> a -> (b, DualType (->+) a b)
evalD (D bType) = evalGAD bType

djam :: DualType (->+) Double (Double, Double)
djam = Dual jam

bjam :: BackpropType (Double, Double) Double
bjam = jam

f :: GADType k a b -> a -> b
f (GAD op) = fst . op

df :: GADType (DualType (->+)) a b -> a -> b -> a
df op = evalAF . evalDual . snd . evalGAD op

--dfD :: GADType (DualType (->+)) a b -> a -> DualType (->+) a b
dfD op = snd . evalGAD op
------------------------------------------------------------------------

-- apply = D $ \((D op), a) -> (fst $ op a, apply)

ttt :: DualType (->+) (DType a b, a) b
ttt = undefined

--fn :: DType a b -> a -> 

instance Closed DType where
  apply :: DType (DType a b, a) b
  apply = D $ GAD $ \(d, a) -> (fst $ evalD d a, ttt)

  curry = undefined
  uncurry = undefined

instance Closed k => Closed (GADType k) where
  apply :: GADType k (GADType k a b, a) b
  apply = GAD $ \(GAD op, a) -> (fst $ op a, undefined)

  curry = undefined
  uncurry = undefined


--  curry :: (Additive3 a b c) => DType (a, b) c -> DType a (DType b c)
--  curry (D op) = linearD $ \a -> D $ \b -> let (c, op') = op (a, b)
--                                           in (c, f (curry op') a)
-- 
--  uncurry :: DType a (DType b c) -> DType (a, b) c
--  uncurry d = linearD $ \(a, b) -> f (f d a) b

--------------------------------------
--
--instance {-# OVERLAPS #-} Additive a => Additive (DType a a) where 
---- does this instance even make sense? Perhaps just zero is needed and it's not "additive"
--  zero = id
--  one = undefined
--  (^+) = undefined
--
--
--


-- Tensor manipulations

ds # cs = listArray ds cs :: Array Double

sh x = putStr P.. formatFixed 2 $ x -- pretty print the tensor

t1 = [2, 3] # [1, 2..] ! "ij"
t2 = [3, 5] # [1, 1..] ! "jk"

t3 = f mul (t1, t2) -- works out of the box with tensor product (since tensor product is a natural generalization of multiplication)

dt1 = df mul (t1, t2)
