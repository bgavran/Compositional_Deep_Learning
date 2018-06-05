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
             UndecidableInstances 
                            #-}

module AD where

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

import CategoricDefinitions

newtype GADType k a b = GAD {
  evalGAD :: a -> (b, a `k` b) 
}

linearD :: (a -> b) -> (a `k` b)-> GADType k a b
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
  type AllowedCoCar (GADType k) a = (AllowedCoCar k a, Allowed (GADType k) a)

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

------------------------------------------------------------------------

f :: GADType k a b -> a -> b
f (GAD op) = fst . op


newtype DualType k a b = Dual {
  evalDual :: b `k` a
}

instance Category k => Category (DualType k) where
  type Allowed (DualType k) a = Allowed k a

  id = Dual id
  Dual g . Dual f = Dual (f . g)

instance Monoidal k => Monoidal (DualType k) where 
  Dual f `x` Dual g = Dual (f `x` g)

instance (Monoidal k, Cocartesian k) => Cartesian (DualType k) where
  type AllowedCar (DualType k) a = AllowedCoCar k a

  exl = Dual inl
  exr = Dual inr
  dup = Dual jam

instance Cartesian k => Cocartesian (DualType k) where
  type AllowedCoCar (DualType k) a = AllowedCar k a

  inl = Dual exl
  inr = Dual exr
  jam = Dual dup

instance Scalable k a => Scalable (DualType k) a where
  scale s = Dual (scale s)

------------------------------------------------------------------------

newtype a ->+ b = AddFun {
  evalAF :: a -> b
}

instance Category (->+) where
  type Allowed (->+) a = Additive a
  id = AddFun id
  AddFun g . AddFun f = AddFun (g . f)

instance Monoidal (->+) where
  AddFun f `x` AddFun g = AddFun (f `x` g)

instance Cartesian (->+) where
  exl = AddFun exl
  exr = AddFun exr
  dup = AddFun dup

instance Cocartesian (->+) where
  inl = AddFun inlF
  inr = AddFun inrF
  jam = AddFun jamF

instance Num a => Scalable (->+) a where
  scale a = AddFun $ \da -> a * da

------------------------------------------------------------------------

type BackpropType a b = GADType (DualType (->+)) a b

bjam :: BackpropType (Double, Double) Double
bjam = jam

df :: GADType (DualType (->+)) a b -> a -> b -> a
df op x = evalAF $ evalDual $ snd $ evalGAD op x

------------------------------------------------------------------------

newtype ContType k r a b = Cont ( (b `k` r) -> (a `k` r)) -- a -> b -> r

cont :: (Category k, Allowed3 k a b r) => (a `k` b) -> ContType k r a b
cont f = Cont (. f)

instance Category k => Category (ContType k r) where
  type Allowed (ContType k r) a = Allowed k a
  id = Cont id
  Cont g . Cont f = Cont (f . g)

--instance Monoidal k => Monoidal (ContType k r) where
--  type AllowedMon (ContType k r) a b c d = (AllowedSeq k (a, b) (r, r) r, 
--                                            AllowedSeq k c (c, d) r,
--                                            AllowedSeq k d (c, d) r,
--                                            AllowedMon k a b r r, 
--                                            Allowed k r, 
--                                            Allowed k c,
--                                            Allowed k d,
--                                            AllowedCoCarJam k r,
--                                            AllowedCoCarIn k c d,
--                                            AllowedCoCarIn k d c,
--                                            Cocartesian k
--                                            )
--  (Cont f) `x` (Cont g) = Cont $ join . (f `x` g) . unjoin
--
--instance Cartesian k => Cartesian (ContType k r) where
--  type AllowedCarEx (ContType k r) a b = ()
--  type AllowedCarDup (ContType k r) a = (AllowedSeq k a (a, a) r,
--                                         Allowed k a,
--                                         AllowedCoCarIn k a a,
--                                         Cocartesian k
--                                        )
--
--  exl = Cont $ undefined
--  exr = Cont $ undefined
--  dup = Cont $ undefined
--
--instance Cocartesian k => Cocartesian (ContType k r) where
--  type AllowedCoCarIn (ContType k r) a b = ()
--  type AllowedCoCarJam (ContType k r) a = (AllowedSeq k (a, a) (r, r) r,
--                                           Allowed k r,
--                                           AllowedMon k a a r r,
--                                           Allowed k a,
--                                           AllowedCoCarJam k r,
--                                           Monoidal k)
--  inl = Cont $ undefined
--  inr = Cont $ undefined
--  jam = Cont $ join . dup
--
--------------------------------------

applyk :: (a `k` b, a) `k` b
applyk = undefined

tt :: Closed k => (GADType k a b, a) `k` b
tt = undefined

instance Closed k => Closed (GADType k) where
  apply :: GADType k (GADType k a b, a) b
  apply = GAD $ \(GAD op, a) -> (fst $ op a, tt)

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
