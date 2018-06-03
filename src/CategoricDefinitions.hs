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

module CategoricDefinitions where

import Prelude hiding ((.))
import qualified Prelude as P
import GHC.Exts (Constraint)


class NoConstraint a where

instance NoConstraint a where

class Category k where
  type Allowed k a :: Constraint
  type Allowed k a = ()

  type AllowedSeq k a b c :: Constraint
  type AllowedSeq k a b c = Allowed3 k a b c

  id  :: Allowed k a => a `k` a
  (.) :: AllowedSeq k a b c => (b `k` c) -> (a `k` b) -> (a `k` c)

class Category k => Monoidal k where
  type AllowedMon k a b c d :: Constraint
  type AllowedMon k a b c d = Allowed4 k a b c d

  x :: AllowedMon k a b c d => (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d)) 


class Monoidal k => Cartesian k where
  type AllowedCarEx k a b :: Constraint
  type AllowedCarEx k a b = ()
  
  type AllowedCarDup k a :: Constraint
  type AllowedCarDup k a = ()

  exl :: AllowedCarEx k a b => (a, b) `k` a
  exr :: AllowedCarEx k a b => (a, b) `k` b
  dup :: AllowedCarDup k a => a `k` (a, a)

class Category k => Cocartesian k where
  type AllowedCoCarIn k a b :: Constraint
  type AllowedCoCarIn k a b = ()

  type AllowedCoCarJam k a :: Constraint
  type AllowedCoCarJam k a = ()

  inl :: AllowedCoCarIn k b a => a `k` (a, b)
  inr :: AllowedCoCarIn k a b => b `k` (a, b)
  jam :: AllowedCoCarJam k a => (a, a) `k` a

class Cartesian k => Closed k where
  apply :: (a `k` b, a) `k` b
  curry :: AllowedSeq k a b c => ((a, b) `k` c) -> a `k` (b `k` c)
  uncurry :: a `k` (b `k` c) -> (a, b) `k` c

(/\) :: (Cartesian k, AllowedSeq k b (b, b) (c, d), AllowedMon k b b c d, AllowedCarDup k b) 
     => b `k` c -> b `k` d -> b `k` (c, d)
f /\ g = (f `x` g) . dup 

(\/) :: (Cocartesian k, Monoidal k, AllowedSeq k (a, b) (c, c) c, Allowed k c, AllowedMon k a b c c, AllowedCoCarJam k c) 
     => a `k` c -> b `k` c -> (a, b) `k` c
f \/ g = jam . (f `x` g)


---- Uncurried versions of the above ops and their inverses
fork :: (Cartesian k, AllowedSeq k b (b, b) (c, d), AllowedMon k b b c d, AllowedCarDup k b) 
     => (b `k` c, b `k` d) -> b `k` (c, d)
fork (f, g) = f /\ g

unfork :: (Cartesian k, AllowedSeq k b (c, d) c, AllowedSeq k b (c, d) d, AllowedCarEx k c d) 
       => b `k` (c, d) -> (b `k` c, b `k` d)
unfork h = (exl . h, exr . h)

join :: (Cocartesian k, Monoidal k, AllowedSeq k (a, b) (c, c) c, Allowed k c, AllowedMon k a b c c, AllowedCoCarJam k c) 
     => (a `k` c, b `k` c) -> (a, b) `k` c
join (f, g) = f \/ g

unjoin :: (Cocartesian k, Monoidal k, AllowedSeq k a (a, b) c, AllowedSeq k b (a, b) c, Allowed k a, Allowed k b, AllowedCoCarIn k b a, AllowedCoCarIn k a b) 
       => (a, b) `k` c -> (a `k` c, b `k` c) 
unjoin h = (h . inl, h . inr)

--------------------------------------

class Additive a where
  zero :: a
  one :: a
  (^+) :: a -> a -> a

type Allowed2 k a b = (Allowed k a, Allowed k b)
type Allowed3 k a b c = (Allowed2 k a b, Allowed k c)
type Allowed4 k a b c d = (Allowed3 k a b c, Allowed k d)
type Allowed5 k a b c d e = (Allowed4 k a b c d, Allowed k e)

type Additive2 a b = (Additive a, Additive b)
type Additive3 a b c = (Additive2 a b, Additive c)
type Additive4 a b c d = (Additive3 a b c, Additive d)
type Additive5 a b c d e = (Additive4 a b c d, Additive e)
type Additive6 a b c d e f = (Additive5 a b c d e, Additive f)
type Additive7 a b c d e f g = (Additive6 a b c d e f, Additive g)
type Additive8 a b c d e f g h = (Additive7 a b c d e f g, Additive h)
type Additive9 a b c d e f g h i = (Additive8 a b c d e f g h , Additive i)
-------------------------------------
-- Instances
-------------------------------------

instance Category (->) where
  id    = \a -> a
  g . f = \a -> g (f a)

instance Monoidal (->) where
  f `x` g = \(a, b) -> (f a, g b)

instance Cartesian (->) where
  exl = \(a, _) -> a
  exr = \(_, b) -> b
  dup = \a -> (a, a)

instance Closed (->) where
  apply (f, a) = f a
  curry f      = \a b -> f (a, b)
  uncurry f    = \(a, b) -> f a b

-------------------------------------

instance {-# OVERLAPS #-} (Num a) => Additive a where
  zero = 0
  one = 1
  (^+) = (+)

instance {-# OVERLAPS #-} (Additive a, Additive b) => Additive (a, b) where
  zero = (zero, zero)
  one = (one, one)
  (a1, b1) ^+ (a2, b2) = (a1 ^+ a2, b1 ^+ b2)

inlF :: Additive b => a -> (a, b)
inlF = \a -> (a, zero)

inrF :: Additive a => b -> (a, b)
inrF = \b -> (zero, b)

jamF :: Additive a => (a, a) -> a
jamF = \(a, b) -> a ^+ b
