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

module Dual where

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P

import CategoricDefinitions

newtype DualType k a b = Dual {
  evalDual :: b `k` a
}

instance Category k => Category (DualType k) where
  type Allowed (DualType k) a = Allowed k a

  id = Dual id
  Dual g . Dual f = Dual (f . g)

instance Monoidal k => Monoidal (DualType k) where
  Dual f `x` Dual g = Dual (f `x` g)
  assocL = Dual assocR
  assocR = Dual assocL
  swap = Dual swap

instance (Cartesian k, Cocartesian k) => Cartesian (DualType k) where
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

instance FloatCat k s => FloatCat (DualType k) s where
  expC = Dual expC
