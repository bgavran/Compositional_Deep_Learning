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

module Para where

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P
import Control.Comonad

import CategoricDefinitions
import GAD


newtype Para p a b = Para {
  evalPara :: DType (p, a) b
}

comp :: Para q b c -> Para p a b -> Para (p, q) a c
comp g f = undefined

{-
-- this is partially defined?
instance Comonad Z where
  extract (P p) = p
  duplicate (P p) = P (P p)

class Parametrizable k where
  fromZ :: (Z a) `k` a

instance Parametrizable (->+) where
  fromZ = AddFun extract

instance Parametrizable k => Parametrizable (DualType k) where
  fromZ = Dual _

instance Parametrizable k => Parametrizable (GADType k) where
  fromZ = linearD extract fromZ

--fromZ :: GADType (->+) (Z a) a
--fromZ = linearD extract (AddFun extract)


dFunctor :: Additive3 p a b => DType a b -> DType (p, a) (p, b)
dFunctor (GAD f) = GAD $ \(p, a) -> let (b, f') = f a
                                    in ((p, b), id `x` f')

delt :: DType (Z p, a) (Z p, (Z p, a))
delt = linearD $ \(p `X` q, a) -> (p, (q, a))


dupl :: DType (Z p) (Z p, Z p)
dupl = linearD $ \(p `X` q) -> (p, q)

{-

Swap map for mon. product of parametrized functions
(a b) (c d)
a (b, (c, d))
a ((b, c), d)
a ((c, b), d)
a (c, (b, d))
(a c) (b d)
-}
swapParam :: Additive4 a b c d => DType ((a, b), (c, d)) ((a, c), (b, d))
swapParam = assocR . (id `x` assocL) . (id `x` (swap `x` id)) . (id `x` assocR) .  assocL

comp :: Additive4 (Z p) a b c => DType (Z p, b) c -> DType (Z p, a) b -> DType (Z p, a) c
g `comp` f = g . (dFunctor f) . delt

xx :: Additive5 (Z p) a b c d => DType (Z p, a) c -> DType (Z p, b) d -> DType (Z p, (a, b)) (c, d)
f `xx` g = (f `x` g) . swapParam . (dupl `x` id)


newtype Para p a b = Para {
  evalPara :: DType (Z p, a) b
}

instance Category (Para p) where
  type Allowed (Para p) x = Additive2 (Z p) x
  id = Para (leftToZ exr)
  (Para g) . (Para f) = Para (g `comp` f)


instance Monoidal (Para p) where
  (Para g) `x` (Para f) = Para (g `xx` f)

-}
