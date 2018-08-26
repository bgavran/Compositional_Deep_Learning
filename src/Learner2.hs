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

module Learner2 where

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P
import Control.Comonad

import CategoricDefinitions
import AD

data Z p
  = NoP
  | P p
  | Z p `X` Z p
  deriving (Eq, Show, Functor)

instance Applicative Z where
  pure = P
  NoP       <*> p = NoP
  (P f)     <*> p = fmap f p
  (f `X` g) <*> p = (f <*> p) `X` (g <*> p)

leftToZ :: DType (p, a) b -> DType (Z p, a) b
leftToZ (D op) = D $ \(P p, a) -> let (b, opD') = op (p, a)
                                  in (b, leftToZ opD')

--instance Category DType where
--  type Allowed DType x = Additive x
--  id      = linearD id
--  D g . D f   = D $ \a -> let (b, f') = f a
--                              (c, g') = g b
--                          in (c, g' . f')

dFunctor :: Additive p => DType a b -> DType (p, a) (p, b)
dFunctor (D f) = D $ \(p, a) -> let (b, f') = f a
                                in ((p, b), id `x` f')

delt :: DType (Z p, a) (Z p, (Z p, a))
delt = linearD $ \(p `X` q, a) -> (p, (q, a))

dd :: Additive4 (Z p) a b c => DType (Z p, b) c -> DType (Z p, a) b -> DType (Z p, a) c
dd g f = g . (dFunctor f) . delt

comp :: DType (Z p, b) c -> DType (Z p, a) b -> DType (Z p, a) c
(D g) `comp` (D f) = D $ \(p `X` q, a) -> let (b, f') = f (p, a)
                                              (c, g') = g (q, b)
                                          in (c, g' `comp` f')


dupl :: DType (Z p) (Z p, Z p)
dupl = linearD $ \(p `X` q) -> (p, q)

swap :: DType (a, b) (b, a)
swap = linearD $ \(a, b) -> (b, a)

assocL :: DType ((a, b), c) (a, (b, c))
assocL = linearD $ \((a, b), c) -> (a, (b, c))

assocR :: DType (a, (b, c)) ((a, b), c)
assocR = linearD $ \(a, (b, c)) -> ((a, b), c)

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

mmm :: Additive3 (Z p) a b => DType (Z p, (a, b)) ((Z p, a), (Z p, b))
mmm = swapParam . (dupl `x` id)

mm :: Additive5 (Z p) a b c d => DType (Z p, a) c -> DType (Z p, b) d -> DType (Z p, (a, b)) (c, d)
f `mm` g = (f `x` g) . mmm


xx :: DType (Z p, a) c -> DType (Z p, b) d -> DType (Z p, (a, b)) (c, d)
(D f) `xx` (D g) = D $ \(p `X` q, (a, b)) -> let (c, f') = f (p, a)
                                                 (d, g') = g (q, b)
                                             in ((c, d), f' `xx` g')

f :: DType (Z p, a) c
f = undefined

g :: DType (Z p, b) d
g = undefined

newtype Para p a b = Para {
  evalPara :: DType (Z p, a) b
}

instance Category (Para p) where
  type Allowed (Para p) x = Additive2 (Z p) x
  id = Para (leftToZ exr)
  (Para g) . (Para f) = Para (g `dd` f)


instance Monoidal (Para p) where
  (Para g) `x` (Para f) = Para (g `xx` f)

