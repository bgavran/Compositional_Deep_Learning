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

module Additive where

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P

import CategoricDefinitions

newtype a ->+ b = AddFun {
  evalAF :: a -> b
}

instance Category (->+) where
  type Allowed (->+) a = Additive a
  id = AddFun id
  AddFun g . AddFun f = AddFun (g . f)

instance Monoidal (->+) where
  AddFun f `x` AddFun g = AddFun (f `x` g)
  assocL = AddFun assocL
  assocR = AddFun assocR
  swap = AddFun swap

instance Cartesian (->+) where
  exl = AddFun exl
  exr = AddFun exr
  dup = AddFun dup

instance Cocartesian (->+) where
  inl = AddFun inlF
  inr = AddFun inrF
  jam = AddFun jamF

instance Num a => Scalable (->+) a where
  scale a = AddFun (*a)

instance Num a => NumCat (->+) a where
  negateC = AddFun negateC
  addC = AddFun addC
  mulC = AddFun mulC

instance Floating a => FloatCat (->+) a where
  expC = AddFun expC
