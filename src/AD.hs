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

import CategoricDefinitions

newtype DType a b = D {
  evalD :: a -> (b, DType a b) -- D a b is here instead of a -> b because sometimes we'd like to have higher order gradients
}

linearD :: (x -> y) -> DType x y
linearD f = D $ \x -> (f x, linearD f) 


instance Category DType where
  type Allowed DType x = Additive x
  id      = D $ \a -> (a, id)
  D g . D f   = D $ \a -> let (b, f') = f a
                              (c, g') = g b
                          in (c, g' . f')

instance Monoidal DType where
  D f `x` D g = D $ \(a, b) -> let (c, f') = f a
                                   (d, g') = g b
                               in ((c, d), f' `x` g')

instance Cartesian DType where
  exl = linearD exl
  exr = linearD exr
  dup = linearD dup

instance Cocartesian DType where
  type AllowedCoCarIn DType a b = Additive a
  type AllowedCoCarJam DType a = Additive a

  inl = linearD inlF
  inr = linearD inrF
  jam = linearD jamF

f :: DType a b -> a -> b
f (D op) = fst . op

instance Closed DType where
  apply :: DType (DType a b, a) b
  apply = linearD (uncurry f)

  curry :: (Additive3 a b c) => DType (a, b) c -> DType a (DType b c)
  curry (D op) = linearD $ \a -> D $ \b -> let (c, op') = op (a, b)
                                           in (c, f (curry op') a)
 
  uncurry :: DType a (DType b c) -> DType (a, b) c
  uncurry d = linearD $ \(a, b) -> f (f d a) b



lcc :: (Additive3 a b c) => DType (a, b) c -> a -> (DType b c)
lcc (D op) = \a -> D $ \b -> let (c, op') = op (a, b)
                             in (c, f (curry op') a)

--lucc :: DType a (DType b c) -> (a, b) -> c
lucc d = \(a, b) -> apply (apply (d, a), b)


cc' :: (Additive3 a b c) => DType (a, b) c -> DType a (DType b c)
cc'  = linearD . lcc


--ucc :: DType a (DType b c) -> DType (a, b) c
--ucc = linearD . lucc

------------------------------------

instance {-# OVERLAPS #-} Additive a => Additive (DType a a) where 
-- does this instance even make sense? Perhaps just zero is needed and it's not "additive"
  zero = id
  one = undefined
  (^+) = undefined


------------------------------------------------------------------------


newtype ContType k r a b = Cont ( (b `k` r) -> (a `k` r)) -- a -> b -> r

cont :: (Category k, AllowedSeq k a b r) => (a `k` b) -> ContType k r a b
cont f = Cont (. f)

instance Category k => Category (ContType k r) where
  type Allowed (ContType k r) a = Allowed k a
  id = Cont id
  Cont g . Cont f = Cont (f . g)

instance Monoidal k => Monoidal (ContType k r) where
  type AllowedMon (ContType k r) a b c d = (AllowedSeq k (a, b) (r, r) r, 
                                            AllowedSeq k c (c, d) r,
                                            AllowedSeq k d (c, d) r,
                                            AllowedMon k a b r r, 
                                            Allowed k r, 
                                            Allowed k c,
                                            Allowed k d,
                                            AllowedCoCarJam k r,
                                            AllowedCoCarIn k c d,
                                            AllowedCoCarIn k d c,
                                            Cocartesian k
                                            )
  (Cont f) `x` (Cont g) = Cont $ join . (f `x` g) . unjoin

instance Cartesian k => Cartesian (ContType k r) where
  type AllowedCarEx (ContType k r) a b = ()
  type AllowedCarDup (ContType k r) a = (AllowedSeq k a (a, a) r,
                                         Allowed k a,
                                         AllowedCoCarIn k a a,
                                         Cocartesian k
                                        )

  exl = Cont $ undefined
  exr = Cont $ undefined
  dup = Cont $ undefined

instance Cocartesian k => Cocartesian (ContType k r) where
  type AllowedCoCarIn (ContType k r) a b = ()
  type AllowedCoCarJam (ContType k r) a = (AllowedSeq k (a, a) (r, r) r,
                                           Allowed k r,
                                           AllowedMon k a a r r,
                                           Allowed k a,
                                           AllowedCoCarJam k r,
                                           Monoidal k)
  inl = Cont $ undefined
  inr = Cont $ undefined
  jam = Cont $ join . dup

------------------------------------

newtype DualType k a b = Dual {
  evalDual :: b `k` a
}

instance Category k => Category (DualType k) where
  type Allowed (DualType k) a = Allowed k a
  type AllowedSeq (DualType k) a b c = AllowedSeq k c b a

  id = Dual id
  Dual g . Dual f = Dual (f . g)

instance Monoidal k => Monoidal (DualType k) where 
  type AllowedMon (DualType k) a b c d = AllowedMon k c d a b

  Dual f `x` Dual g = Dual (f `x` g)

instance Cartesian k => Cartesian (DualType k) where
  type AllowedCarEx (DualType k) a b = (Cocartesian k, AllowedCoCarIn k b a, AllowedCoCarIn k a b)
  type AllowedCarDup (DualType k) a = (Cocartesian k, AllowedCoCarJam k a)
  
  exl = Dual inl
  exr = Dual inr
  dup = Dual jam

instance Cocartesian k => Cocartesian (DualType k) where
  type AllowedCoCarIn (DualType k) a b = (Cartesian k, AllowedCarEx k a b, AllowedCarEx k b a)
  type AllowedCoCarJam (DualType k) a = (Cartesian k, AllowedCarDup k a)

  inl = Dual exl
  inr = Dual exr
  jam = Dual dup
