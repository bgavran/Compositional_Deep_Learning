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

module Para where

import Prelude hiding (id, (.))
import Control.Comonad

import CategoricDefinitions
import GAD
import Dual
import Additive

-- perhaps this class isn't even needed
class Monoidal k => Parametrizable k where
  comp :: (Allowed k ((p, q), a), Allowed k ((q, p), a), Allowed k c, Allowed k (q, (p, a)), Allowed k (q, b), Allowed k q,
      Allowed k (p, a), Allowed k b, Allowed k p, Allowed k a, Allowed k (q, p), Allowed k (p, q))
          => (q, b) `k` c -> (p, a) `k` b -> ((p, q), a) `k` c


instance Monoidal k => Parametrizable (GADType k) where
  g `comp` f = g . (id `x` f) . assocL . (swap `x` id)

----------------------------------

newtype DType a b = D {
  evalDType :: GADType (DualType (->+)) a b
} deriving (Category, Monoidal, Cartesian, Cocartesian, Parametrizable)

instance Num s => NumCat DType s where
  negateC = D negateC
  addC = D addC
  mulC = D mulC

instance Floating s => FloatCat DType s where
  expC = D expC

----------------------------------

data PType p
  = P p
  | (PType p) `X` (PType p)
  deriving (Eq, Show, Functor)

-- there is this problem of no dependent types in haskell which the following function highlights
-- With dependent types it should be possible to tell that we can only convert to tuple something that's a _product_, not a normal value
-- how far away is this from the COMONAD idea? 'duplcate' was the previos name
toTuple :: PType p -> (PType p, PType p)
toTuple (p `X` q) = (p, q)
toTuple (P p) = error "nope!"

dToTuple :: DType (PType p) (PType p, PType p)
dToTuple = D $ linearD toTuple (Dual (AddFun (uncurry X)))

{-
Swap map for mon. product of parametrized functions, from top to bottom
(a b) (c d)
a (b, (c, d))
a ((b, c), d)
a ((c, b), d)
a (c, (b, d))
(a c) (b d)

(Monoidal k, aBunchOfConstraints) =>
swapParam :: ((a, b), (c, d)) `k` ((a, c), (b, d))
-}
swapParam = assocR . (id `x` assocL) . (id `x` (swap `x` id)) . (id `x` assocR) .  assocL

----------------------------------

newtype Para p a b = Para {
  evalPara :: DType (PType p, a) b
}


instance Category (Para p) where
  type Allowed (Para p) a = Allowed2 (DType) (PType p) a

  id = Para exr
  (Para g) . (Para f) = Para $ (g `comp` f) . (dToTuple `x` id)


instance Monoidal (Para p) where
  (Para f) `x` (Para g) = Para $ (f `x` g) . swapParam . (dToTuple `x` id)
  assocL = Para $ exr . (id `x` assocL) -- also possible: (unitorL . (counit `x` assocR)), which is better?
  assocR = Para $ exr . (id `x` assocR)
  swap = Para $ exr . (id `x` swap)

fp :: Para p a b -> (PType p, a) -> b
fp = fGAD . evalDType . evalPara

dfp :: Para p a b -> (PType p, a) -> b -> (PType p, a)
dfp para inp = evalGrad $ (dfGAD . evalDType . evalPara) para inp

f :: DType a b -> a -> b
f = fGAD . evalDType

df :: DType a b -> a -> b -> a
df para inp = evalGrad $ (dfGAD . evalDType) para inp

myf :: (Additive a, Floating a) => DType (a, a) a
myf = jam . (expC `x` mulC) . assocL . (dup `x` id)


















{-
-- this is partially defined?
instance Comonad Z where
  extract (P p) = p
  duplicate (P p) = P (P p)

class Parametrizable k where
  fromZ :: (Z a) `k` a

instance Parametrizable (->+) where
  fromZ = AddFun extract

instance Parametrizable k => Parametrizable (GADType k) where
  fromZ = linearD extract fromZ

--fromZ :: GADType (->+) (Z a) a
--fromZ = linearD extract (AddFun extract)

dFunctor :: Additive3 p a b => DType a b -> DType (p, a) (p, b)
dFunctor (GAD f) = GAD $ \(p, a) -> let (b, f') = f a
                                    in ((p, b), id `x` f')

delt :: DType (Z p, a) (Z p, (Z p, a))
delt = linearD $ \(p `X` q, a) -> (p, (q, a))

comp :: Additive4 (Z p) a b c => DType (Z p, b) c -> DType (Z p, a) b -> DType (Z p, a) c
g `comp` f = g . (dFunctor f) . delt
-}
