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

module Cont where

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P

import CategoricDefinitions

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
