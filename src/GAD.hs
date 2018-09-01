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
             GeneralizedNewtypeDeriving,
             AllowAmbiguousTypes
                            #-}

module GAD where

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P
import Control.Comonad
--import Numeric.LinearAlgebra.Array
--import Numeric.LinearAlgebra.Array.Util

import CategoricDefinitions
import Additive
import Dual
import Cont

newtype GADType k a b = GAD {
  evalGAD :: a -> (b, a `k` b)
}

type LinType a b = GADType (->+) a b

type DType a b = GADType (DualType (->+)) a b

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
  swap = linearD swap swap

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

-- Can the set of these constraints be shortened?
-- negateC and addC differ from paper; 2nd argument to linearD is changed so the constraint NumCat k s isn't needed anymore
instance (Num s, Scalable k s, Monoidal k, Cocartesian k,
          Allowed k (s, s), Allowed k s, AllowedCoCar k s) => NumCat (GADType k) s where
  negateC = linearD negateC (scale (-1)) -- this is where
  addC = linearD addC jam
  mulC = GAD $ \(a, b) -> (a * b, scale b \/ scale a) -- most of the instance constraints come from \/

instance (Floating s, FloatCat k s, Scalable k s) => FloatCat (GADType k) s where
  expC = GAD $ \a -> let e = exp a
                     in (e, scale e)

f :: GADType k a b -> a -> b
f (GAD op) = fst . op

df :: GADType k a b -> a -> k a b
df (GAD op) = snd . op

sqr :: (Additive a, Num a) => GADType (DualType (->+)) a a
sqr = mulC . dup

myf :: (Additive a, Floating a) => GADType (DualType (->+)) a a
myf = expC . sqr

sv :: (Additive a, Num a) => a -> a
sv = f sqr

dsv :: (Additive a, Num a) => a -> DualType (->+) a a
dsv = df sqr

m :: (Num a, Additive a) => GADType (DualType (->+)) (a, a) a
m = mulC

dm :: (Num a, Additive a) => (a, a) -> DualType (->+) (a, a) a
dm = df mulC

a :: (Num a, Additive a) => DualType (->+) (a, a) a
a = dm (2, 3)

b :: (Num a, Additive a) => a ->+ (a, a)
b = evalDual a

c :: (Num a, Additive a) => a -> (a, a)
c = evalAF b
