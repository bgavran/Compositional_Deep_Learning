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

module GAD where

import Prelude hiding (id, (.))
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
  --unitorL = linearD unitorL unitorL
  --unitorL' = linearD unitorL' unitorL'
  --unitorR = linearD unitorR unitorR
  --unitorR' = linearD unitorR' unitorR'
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

instance (Floating s, FloatCat k s, Scalable k s) => FloatCat (GADType k) s where
  expC = GAD $ \a -> let e = exp a
                     in (e, scale e)

fGAD :: GADType k a b -> a -> b
fGAD (GAD op) = fst . op

dfGAD :: GADType k a b -> a -> k a b
dfGAD (GAD op) = snd . op

evalGrad :: DualType (->+) a b -> b -> a
evalGrad = evalAF . evalDual
