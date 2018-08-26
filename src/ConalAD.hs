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

module ConalAD where

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P
--import Numeric.LinearAlgebra.Array
--import Numeric.LinearAlgebra.Array.Util

import CategoricDefinitions
import Additive
import Dual
import Cont

newtype GADType k a b = GAD {
  evalGAD :: a -> (b, a `k` b) 
}

newtype DType a b = D {
  evalD :: GADType DType a b
}

type BackpropType a b = GADType (DualType (->+)) a b

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

--instance Scalable k s => (GADType k) where
  
instance (Num s, NumCat k s, Scalable k s) => NumCat (GADType k) s where
  negateC = linearD negateC negateC
  addC = linearD addC addC
  mulC = undefined
  --mulC = GAD $ \(a, b) -> (a * b, scale b \/ scale a) -- \/ has a bunch of extra constraints?

ttt :: (GADType k a b, a) `k` b
ttt = undefined

applyk :: (a `k` b, a) `k` b
applyk = undefined

instance Closed k => Closed (GADType k) where
  apply :: (GADType k a b, a) -> b
  apply (GAD op, a) = fst $ op a

--  curry :: GADType k (a, b) c -> GADType k a (GADType k b c)
--  curry d@(GAD op) = GAD $ \a -> (GAD $ \b -> let (c, op') = op (a, b)
--                                            in (c, apply (curry op', a)), df (curry d) a)
--
--  uncurry :: GADType k a (GADType k b c) -> GADType k (a, b) c
--  uncurry d = GAD $ \(a, b) -> let c = apply (apply (d, a), b)
--                               in (c, df (uncurry d) (a, b))

xx :: (a, b) `k` c
xx = undefined

ucurapply :: GADType k a (GADType k b c) -> (a, b) -> c
ucurapply g = \(a, b) -> f (f g a) b

f :: GADType k a b -> a -> b
f (GAD op) = fst . op

df :: GADType k a b -> a -> k a b
df (GAD op) = snd . op


--mul :: (Additive a, Num a) => BackpropType (a, a) a
mul = GAD $ \(a, b) -> (a * b, scale b \/ scale a)

expC :: (Floating a, Scalable k a) => GADType k a a
expC = GAD $ \a -> let e = exp a
                   in (e, scale e)

emul = df mul (2, 3)

sqr :: (Additive a, Num a) => GADType (DualType (->+)) a a
sqr = mul . dup

myf :: (Additive a, Floating a) => GADType (DualType (->+)) a a
myf = expC . sqr
